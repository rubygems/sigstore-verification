# frozen_string_literal: true

require "literal"
module Sigstore::Verification
  module ToAST
    def to_ast
      [self.class.name, *to_h.reject { %i[end start source].include?(_1) }.transform_values! do |v|
        case v
        in ToAST
          v.to_ast
        in Array
          v.map(&:to_ast)
        in Hash
          v.transform_values { |v| v.is_a?(ToAST) ? v.to_ast : v }.to_a
        else
          v
        end
      end.compact.to_a]
    end

    def attributes_for_super_diff
      to_h.reject { %i[end start source].include?(_1) }.compact
    end
  end

  class PolicySet < Literal::Struct
    include ToAST

    def self._Lazy(type)
      ->(o) { !o.nil? && const_get(type) === o }
    end

    prop :source, _String?
    prop :filename, _String?
    prop :description, _String?
    prop :policies, _Array(_Lazy("Policy")), default: -> { [] }
    prop :defs, _Hash(String, _Lazy("Macro")), default: -> { {} }
    def evaluate(subject)
      ctx = Context.new(policy_set: self, this: subject, vars: {})
      policies.each do |policy|
        next unless policy.type === subject["type"] && policy.name === subject["name"]

        policy.evaluate(ctx)
      end
      ctx
    end
  end

  class Context < Literal::Struct
    prop :policy_set, PolicySet
    prop :this, _JSONData
    prop :vars, _Hash(String, _Any)

    prop :matching_policies, _Hash(PolicySet._Lazy("Policy"), _Hash(String, _Any)), default: -> { {} }

    def result
      failures = matching_policies.reject { |_, z| z.empty? }
      [matching_policies.any? && failures.empty?, failures]
    end

    def inside_policy(policy)
      matching_policies[[policy[:start], policy[:end]]] = {}
      self
    end

    def add_failure(rule)
      matching_policies.to_a.last.last[[rule[:start], rule[:end]]] = rule.deconstruct_in_ctx(self)
    end

    def attributes_for_super_diff
      { matching_policies:, result:, this:, format_failures: }
    end

    def success?
      result.first
    end

    def format_failures
      failures = matching_policies.reject { |_, z| z.empty? }
      failures.map do |(policy_start, policy_end), failing_rules_by_location|
        # TODO: extract line and column from policy_start
        "#{policy_set.filename || "<unknown>"}:#{policy_start}:#{policy_end}:error: policy failed to match\n" +
          policy_set.source.byteslice(policy_start, policy_end - policy_start) + "\n" +
          failing_rules_by_location.map do |(rule_start, rule_end), rule|
            next "  unknown: #{rule.inspect}" if rule_start.nil? || rule_end.nil?

            "  #{policy_set.source.byteslice(rule_start, rule_end - rule_start).chomp}: #{rule.inspect}"
          end.join("\n")
      end.join("\n")
    end
  end

  module Rule
    def self.included(base)
      base.include ToAST
      base.prop :start, base._Nilable(Integer)
      base.prop :end, base._Nilable(Integer)
      base.prop :rules, base._Array(Rule), default: -> { [] }
    end

    def evaluate(subject)
      raise "#{self.class.name}#evaluate not implemented"
    end

    def evaluate_children(ctx)
      rules&.reduce(nil) do |_, rule|
        rule.evaluate(ctx)
      end
    end

    class Error < Literal::Struct
      include Rule
      prop :message, String

      def evaluate(ctx)
        ctx.add_failure(self)
      end

      def deconstruct_in_ctx(ctx)
        { message: }
      end
    end

    class MacroCall < Literal::Struct
      include Rule
      prop :name, String
      prop :props, _Hash(String, _Union(String, Integer, _Boolean))

      def evaluate(ctx)
        macro = ctx.policy_set.defs.fetch(name)
        evaluate_children(ctx)
        macro.evaluate(ctx, props)
      end
    end

    class PropertyValue < Literal::Struct
      include Rule
      prop :name, _Nilable(String)
      prop :value, _Any?

      def evaluate(ctx)
        # (rules.any? ^ !value.nil?) || raise("property value #{name} must have a value or rules")
        if value
          if lhs(ctx) == value.evaluate(ctx)
            true
          else
            ctx.add_failure(self)
            nil
          end
        else
          this = ctx.this

          t = lhs(ctx)
          if t.is_a?(Array) # TODO: add annotation for "any" node
            failures = ctx.matching_policies.to_a.last.last.dup

            t.each do |item|
              failure_count = ctx.matching_policies.to_a.last.last.size
              ctx.this = item
              evaluate_children(ctx)
              next unless ctx.matching_policies.to_a.last.last.size == failure_count

              ctx.matching_policies.to_a.last.last.replace(failures)
              break
            end
          elsif !t.is_a?(Array)
            ctx.this = t
            evaluate_children(ctx)
            t
          else
            ctx.add_failure(self)
          end
        end
      ensure
        ctx.this = this if this
      end

      def lhs(ctx)
        curr = ctx.this
        return curr if name.empty?

        parts = name.split(".")
        i = 0
        while i < parts.size
          n = parts[i]
          v = curr.fetch(n) do
            return [:not_found, curr, n]
          end
          return [:not_a_hash, curr, n] if i < parts.size - 1 && !v.is_a?(Hash)

          curr = v
          i += 1
        end
        curr
      end

      def deconstruct_in_ctx(ctx)
        { property: name, actual: lhs(ctx), expected: value&.deconstruct_in_ctx(ctx) }
      end
    end

    class Variable < Literal::Struct
      include Rule
      prop :name, String

      def evaluate(ctx)
        evaluate_children(ctx)
        return ctx.this if name == "_"

        ctx.vars.fetch(name) do
          ctx.add_failure(self)
          [:not_found, ctx.this, name]
        end
      end

      def deconstruct_in_ctx(ctx)
        { variable: name, value: ctx.vars.fetch(name, :not_found) }
      end
    end

    class Op < Literal::Struct
      include Rule
      prop :name, String
      prop :args, _Array(Rule)

      def evaluate(ctx)
        evaluate_children(ctx)

        return ctx.add_failure(self) if args.nil?

        a = args.map { _1.evaluate(ctx) }
        string_array = self.class._Array(String)
        case [name, a]
        in ["+", string_array]
          a.join
        else
          ctx.add_failure(self)
          nil
        end
      end

      def deconstruct_in_ctx(ctx)
        { op: name, args: args.map { _1.deconstruct_in_ctx(ctx) } }
      end
    end

    class Lit < Literal::Struct
      include Rule
      prop :value, _Any?

      def evaluate(ctx)
        evaluate_children(ctx)
        value
      end

      def deconstruct_in_ctx(ctx)
        value
      end
    end

    class VarAssignment < Literal::Struct
      include Rule
      prop :name, String

      def evaluate(ctx)
        res = evaluate_children(ctx)
        return ctx.add_failure(self) if ctx.vars.key?(name)

        ctx.vars[name] = res
      end
    end
  end

  class Policy < Literal::Struct
    include ToAST
    prop :start, _Nilable(Integer)
    prop :end, _Nilable(Integer)

    prop :type, String
    prop :name, _Union(String, Regexp)
    prop :effect, _String?
    prop :rules, _Array(Rule), default: -> { [] }

    def evaluate(ctx)
      ctx.inside_policy(self)
      rules.each do |rule|
        rule.evaluate(ctx)
      end
    end
  end

  class Macro < Literal::Struct
    include ToAST

    prop :start, _Nilable(Integer)
    prop :end, _Nilable(Integer)

    prop :name, String
    prop :props, _Hash(String, String)
    prop :rules, _Array(Rule), default: -> { [] }

    def evaluate(ctx, props)
      vars = ctx.vars.dup
      props.each do |name, val|
        type = self.props[name] || raise("unknown property #{name}")
        value = val.evaluate(ctx)
        case type
        when Type::INTEGER
          raise TypeError, "expected integer for #{name}, got #{value.inspect}" unless value.is_a?(Integer)
        when Type::STRING
          raise TypeError, "expected string for #{name}, got #{value.inspect}" unless value.is_a?(String)
        when Type::BOOLEAN
          raise TypeError, "expected boolean for #{name}, got #{value.inspect}" unless [true, false].include?(value)
        when Type::INTEGER_P
          unless value.nil? || value.is_a?(Integer)
            raise TypeError,
                  "expected integer or nil for #{name}, got #{value.inspect}"
          end
        when Type::STRING_P
          unless value.nil? || value.is_a?(String)
            raise TypeError,
                  "expected string or nil for #{name}, got #{value.inspect}"
          end
        when Type::BOOLEAN_P
          unless value.nil? || value == true || value == false
            raise TypeError,
                  "expected boolean or nil for #{name}, got #{value.inspect}"
          end
        end
        ctx.vars[name] = value
      end
      rules.each do |rule|
        rule.evaluate(ctx)
      end
    ensure
      ctx.vars = vars
    end

    class Type < Literal::Enum(String)
      INTEGER = new("integer")
      STRING = new("string")
      BOOLEAN = new("boolean")

      INTEGER_P = new("integer?")
      STRING_P = new("string?")
      BOOLEAN_P = new("boolean?")

      # def attributes_for_super_diff
      #   { name: name }
      # end
    end
  end
end
