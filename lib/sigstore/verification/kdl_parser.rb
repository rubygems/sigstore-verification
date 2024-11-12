# frozen_string_literal: true

require "strscan"
require "pp"

module Sigstore
  module Verification
    class KDLParser
      def err(scanner, context, message)
        lines = scanner.string.lines
        err_line_number = 0
        pos = scanner.pos
        lines.each do |line|
          break if pos < line.bytesize

          err_line_number += 1

          pos -= line.bytesize
        end
        col = 0
        start_line = [err_line_number - 5, 0].max
        surrounding_lines = lines[start_line, 10].tap do |l|
          l << +"\n" if err_line_number >= lines.size
        end.each_with_index.map do |line, index|
          line.chomp!
          line << "\n"
          if index == (err_line_number - start_line)
            "#{line}#{" " * (col = pos % line.bytesize)}^\n"
          else
            line
          end
        end.join
        raise "Error at #{@filename || "<unknown>"}:#{err_line_number + 1}:#{col} (byte #{scanner.pos}): " \
              "#{message} (in #{context.inspect})\n#{surrounding_lines}"
      end

      NEWLINE = /\r\n|\r|\n|\u0085|\u000C|\u2028|\u2029/
      WS = /[\u0009\u0020\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000]+/
      IDENTIFIER = %r'(([-](?=[^0-9]))|[^0-9+\\/(){}<>;,\[\]=."\n -])[^\x2f\\(){}<>;,\[\]=" \r\n/\u0085\u000C\u2028\u2029\u0009\u0020\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000]*'
      KEYWORDS = %w[null true false].freeze
      RAW_STRING_TERMINATORS = [
        /(?=")/,
        /(?="\#{1})/,
        /(?="\#{2})/,
        /(?="\#{3})/,
        /(?="\#{4})/,
        /(?="\#{5})/,
        /(?="\#{6})/,
        /(?="\#{7})/,
        /(?="\#{8})/
      ].freeze

      def skip_multiline_comment_block(scanner, context)
        return unless scanner.skip(%r{#{WS}?/[*]}o)

        multiline_comment = 1
        until multiline_comment.zero?
          scanner.skip_until(%r{[*]/|/[*]}) || err(scanner, context, "unclosed multiline comment")
          if scanner.string.getbyte(scanner.pos - 1) == 0x2f
            multiline_comment -= 1
          else
            multiline_comment += 1
          end
        end

        true
      end

      def scan_string(scanner)
        if (len = scanner.skip(/r#*"/))
          pounds = len - 2
          value = scanner.scan_until(RAW_STRING_TERMINATORS[pounds] || /(?="\#{#{pounds}})/) ||
                  (
                    scanner.pos = scanner.pos - len + 1
                    err(scanner, :raw_string, "unterminated raw string")
                  )

          # TODO: on failure, see how many pounds we've seen and try to match n-1 pounds, n-2 pounds, etc.
          # for a better error message
          # match forward for a pound and if so, error with a suggestion of how many pounds to use

          scanner.pos += pounds + 1

          return value
        end

        return nil unless scanner.skip(/"/)

        outbuf = nil

        pos = start = scanner.pos

        loop do
          byte = scanner.string.getbyte(pos)
          if byte.nil?
            scanner.pos = start
            err(scanner, :string, "unterminated string")
          end
          pos += 1
          break if byte == 0x22

          next unless byte == 0x5c

          outbuf ||= +""
          outbuf.bytesplice(outbuf.bytesize, 0, scanner.string, start, pos - start - 1)
          byte = scanner.string.getbyte(pos)
          pos += 1
          start = pos

          outbuf << case byte
                    when nil
                      scanner.pos = start - 1
                      err(scanner, :string, "unterminated string")
                    when "t".ord
                      "\t"
                    when "n".ord
                      "\n"
                    when "r".ord
                      "\r"
                    when "f".ord
                      "\f"
                    when "b".ord
                      "\b"
                    when "u".ord
                      scanner.pos = pos
                      len = scanner.skip(/\{[\dA-Fa-f]{1,6}\}/) || err(scanner, :string, "expected unicode escape")
                      # find a way to avoid the byteslice?
                      v = [scanner.string.byteslice(pos + 1, len - 2)].pack("H#{len - 2}")
                      pos += len
                      start = pos
                      v
                    when '"'.ord
                      '"'
                    when "\\".ord
                      "\\"
                    when "/".ord
                      "/"
                    else
                      scanner.pos -= 1
                      err(scanner, :string, "unknown escape sequence #{byte.chr.inspect} #{byte.to_s(16)}")
                    end
        end

        scanner.pos = pos

        if outbuf
          outbuf.bytesplice(outbuf.bytesize, 0, scanner.string, start, pos - start - 1)
        else
          scanner.string.byteslice(start, pos - start - 1)
        end
      end

      def scan_identifier(scanner)
        id = scan_string(scanner)
        return id if id

        id = scanner.scan(IDENTIFIER)
        if id && KEYWORDS.include?(id)
          scanner.pos -= id.bytesize
          err(scanner, :identifier,
              "#{id} is a keyword and is not valid as bare identifier")
        end
        id
      end

      def scan_property_name(scanner)
        pos = scanner.pos
        scanner.skip(WS)
        if (id = scanner.scan(IDENTIFIER)) && scanner.skip("=")
          if KEYWORDS.include?(id)
            scanner.pos -= id.bytesize + 1
            err(scanner, :property, "#{id} is a keyword and is not valid as bare identifier")
          end
          id
        elsif (!id || (scanner.pos -= id.bytesize)) && (id = scan_string(scanner)) && scanner.skip("=")
          id
        else
          scanner.pos = pos
          nil
        end
      end

      def skip_nl(scanner)
        scanner.skip(/#{NEWLINE}/o)
      end

      def scan_type(scanner, context)
        return unless scanner.skip("(")

        type = scan_identifier(scanner) || err(scanner, context, "expected type name")
        scanner.skip(")") || err(scanner, context, "missing close of type name")
        type
      end

      def make_node(type, id, pos)
        id = [type, id] if type
        [id, pos, nil, nil, nil, nil]
      end

      def make_value(type, value, _val_start, _val_end)
        if type
          [type, value]
        else
          value
        end
      end

      def self.parse(string, filename: nil)
        new(string, filename:).parse
      end

      def initialize(string, filename: nil)
        @policy_cache = {}
        @filename = filename
        @scanner = StringScanner.new(string)
      end

      def parse
        context = :nodes
        @nodes = nodes = []
        toplevel = []
        prop_name = nil
        scanner = @scanner
        slapdash = false
        last = -1
        loop do
          break if scanner.eos? && nodes.empty? && !slapdash

          err(scanner, context, "made no progress\n#{nodes.inspect}\n#{scanner.rest.inspect}") if scanner.pos == last
          last = scanner.pos

          next if scanner.bol? && skip_nl(scanner)

          skip_multiline_comment_block(scanner, context)

          if scanner.skip(%r{#{WS}?//}o)
            scanner.skip_until(/#{NEWLINE}|\z/o)
            next
          end

          slapdash = true if scanner.skip(%r{#{WS}?/-#{WS}?}o)
          # TODO: error if EOF instead of newline at the end
          next if scanner.skip(%r{(#{WS}|#{NEWLINE})*\\#{WS}?(?://[^\n]*)?\r?\n}o) # line continuation

          case context
          when :nodes
            next slapdash = false if terminate_node(scanner, closing_needed: true)

            break if scanner.eos? && !slapdash

            next if scanner.skip(/(#{WS}|#{NEWLINE})/o)

            pos = scanner.pos

            type = scan_type(scanner, context)
            unless id = scan_identifier(scanner)
              err(scanner, context, "expected node id but found newline") if scanner.match?(NEWLINE)
              err(scanner, context, "expected node id but found number") if scanner.match?(/[+-]?\d/)
              err(scanner, context, "expected node id")
            end

            context = :node
            n = make_node(type, id, pos)
            if slapdash
              slapdash = false
            else
              add_child(n)
              toplevel << n if nodes.empty?
            end
            nodes << n
          when :node
            if scanner.skip(/#{WS}?\{#{WS}?#{NEWLINE}?/o)
              context = :nodes
            else
              scanner.pos
              if (id = scan_property_name(scanner))
                # TODO: store position of property name start so the prop nodes can have a position?
                prop_name = id
              else
                # TODO: avoid scanning string values twice
                last -= 1 # make it look like we've made progress... it's ok since we moved the context forwards
              end
              context = :value
            end
          when :value
            missing_space = false

            unless prop_name || scanner.skip(/#{WS}/o) || scanner.bol? || scanner.match?(/#{NEWLINE}|\z|;/o) ||
                   slapdash ||
                   # not missing space if preceded by the end of a multiline comment
                   (scanner.string.getbyte(scanner.pos - 1) == 0x2f && scanner.string.getbyte(scanner.pos - 2) == 0x2a)
              missing_space = scanner.pos
            end

            val_start = scanner.pos
            type = scan_type(scanner, context)
            if (value = scan_string(scanner))
              # nil
            elsif (len = scanner.skip(/true|false|null/))
              value =
                if len == 5
                  false
                elsif scanner.string.getbyte(scanner.pos - 1) == 0x65
                  true
                end
            elsif (len = scanner.skip(/[+-]?0b/))
              sign = scanner.string.getbyte(scanner.pos - len)

              unless (value = scanner.scan(/[01][01_]*/)&.to_i(2))
                err(scanner, :binary_number,
                    "0b must be followed by one of [01]")
              end
              value = -value if sign == 45
              unless scanner.match?(/#{WS}|(\\)?#{NEWLINE}|[;{}]|\z/o)
                pos = scanner.pos
                rest = scanner.scan_until(/(?=#{WS}|(\\)?#{NEWLINE}|[;{}]|\z)/o)
                scanner.pos = pos
                err(scanner, :binary_number, "only [01][01_]* allowed in binary number, but found #{rest.inspect}")
              end
            elsif (value = scanner.scan(/[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*/))
              value = value.to_i(16)
            elsif (value = scanner.scan(/[+-]?0o[0-7][0-7_]*/))
              value = value.to_i(8)
            elsif (value = scanner.scan(/([+-]?\d[\d_]*(?:\.(\d[\d_]*))?)(?:[eE]([+-]?\d[\d_]*))?/))
              _, fractional, exponent = scanner.captures
              value = if fractional.nil? && exponent.nil?
                        value.to_i
                      else
                        f = Float(value)
                        if f.infinite? || (f.zero? && exponent)
                          BigDecimal(value, 0)
                        else
                          f
                        end
                      end
            elsif prop_name
              err(scanner, context, "expected value for property #{prop_name.inspect}")
            elsif !type && terminate_node(scanner, closing_needed: false)
              context = :nodes

              next
            elsif scanner.match?(IDENTIFIER)
              err(scanner, context,
                  "plain identifiers can't be used here, expected argument or property for #{nodes.last.to_s.inspect}")
            else
              err(scanner, context, "expected argument or property for #{nodes.last.to_s.inspect}")
            end
            if missing_space
              scanner.pos = missing_space
              err(scanner, context, "missing space before value #{value.inspect}")
            end
            scanner.match?(/#{WS}|(\\)?#{NEWLINE}|[;{}]|\z/o) || err(scanner, context, "expected separator after value")
            context = :node
            if slapdash
              slapdash = false
              prop_name = nil
              next
            end

            value = make_value(type, value, val_start, scanner.pos)

            if prop_name
              add_property(prop_name, value)

              prop_name = nil
            else
              add_argument(value)
            end
          else
            err(scanner, context, "context error")
          end
        end

        err(scanner, :nodes, "unclosed nodes:\n\n#{nodes.map(&:to_s).join("\n")}\n") if nodes.any?

        toplevel
      end

      def add_property(name, value)
        (@nodes.last[4] ||= {})[name] = value
      end

      def add_argument(value)
        (@nodes.last[3] ||= []) << value
      end

      def terminate_node(scanner, closing_needed: false)
        if closing_needed
          return unless scanner.skip(/#{WS}?(#{NEWLINE}|\z)?;?}/o)

          skip_multiline_comment_block(scanner, :node_terminator)

          unless scanner.skip(/;|#{NEWLINE}|\z/o) || scanner.match?(%r{/\*}o)
            err(scanner, nil,
                "Nodes can only be terminated by `;` or a valid line ending")
          end
        else
          return unless scanner.skip(/#{WS}?(;|\z|#{NEWLINE})/o)
        end

        node = @nodes.pop
        node_end(node, scanner.pos)
        node
      end

      def node_end(node, pos)
        node[2] = pos
      end

      def self.format_value(value)
        require "bigdecimal"
        if value in Array[type, v]
          "(#{format_id type})#{format_value v}"
        elsif value.nil?
          "null"
        elsif (value in Float) && (value.abs <= 100)
          value.to_s.tap do |s|
            s.sub!(/\A(-?\d+)e/, "\\1.0E")
            s.sub!(/\A(-?\d+)\.(\d+)e/, "\\1.\\2E")
          end
        elsif value in BigDecimal | Float
          sign, significant_digits, _base, exponent = BigDecimal(value, 0).split
          e = exponent.negative? || exponent >= 6
          str = +""
          str << "-" if sign == -1
          if e
            str << significant_digits[0]
            str << "."
            str << (significant_digits[1..].then { _1 unless _1.empty? } || "0")
            str << "E"
            str << "+" if exponent.positive?
            str << exponent.pred.to_s
          else
            str << (significant_digits[0, exponent].then { _1 unless _1.empty? } || "0").ljust(exponent.abs, "0")
            str << "."
            str << (significant_digits[exponent..]&.then { _1 unless _1.empty? } || "0").ljust(exponent.abs.pred, "0")
          end
          str
        else
          value.inspect
        end
      end

      def self.format_id(id)
        case id
        in [String => type, id]
          "(#{format_id type})#{format_id id}"
        in String if /\A#{IDENTIFIER}\z/o.match?(id)
          id
        in String
          id.inspect
        end
      end

      def self.format_ast(ast)
        ast.map do |id, _start, _fin, args, props, children|
          parts = [format_id(id)]
          args&.each do |a|
            parts << " " << format_value(a)
          end
          props&.each do |k, v|
            parts << " " << "#{format_id(k)}=#{format_value(v)}"
          end
          if children
            parts << " {\n"
            parts << format_ast(children).gsub(/^/, "    ")
            parts << "}"
          end
          parts.join
        end.join("\n") << "\n"
      end

      def add_child(child)
        return if @nodes.empty?

        (@nodes.last[5] ||= []) << child
      end
    end

    class KDLNodeParser < KDLParser
      require "kdl"

      def parse(...)
        toplevel = super
        KDL::Document.new(toplevel)
      end

      def make_node(type, id, _pos)
        KDL::Node.new(id, type:)
      end

      def add_property(name, value)
        @nodes.last.properties[name] = value
      end

      def add_argument(value)
        @nodes.last.arguments << value
      end

      def node_end(node, pos); end

      def self.format_ast(ast)
        ast.to_s
      end

      def make_value(type, value, _val_start, _val_end)
        if value.is_a?(BigDecimal) || (value.is_a?(Float) && (value.to_s.include?("e") || value.abs >= 100))
          format = if value.is_a?(Float)
                     "%.#{BigDecimal(value, 0).then do |decimal|
                       [decimal.exponent > 1 ? decimal.n_significant_digits - 1 : decimal.n_significant_digits,
                        1].max
                     end}E"
                   end
          return KDL::Value::Float.new(value, format:, type:)
        end

        KDL::Value.from(value).as_type(type)
      end

      def add_child(child)
        @nodes.last&.children&.<< child
      end
    end

    class PolicyParser < KDLParser
      def make_node(type, id, pos)
        case current_node
        when nil

          if id == "description"
            raise "Types are not supported in policy files" if type

            @policy_set.description = ""
          elsif type == "def"
            macro = Macro.allocate
            macro.instance_variable_set(:@start, pos)
            macro.instance_variable_set(:@end, nil)
            macro.instance_variable_set(:@props, {})
            macro.instance_variable_set(:@rules, [])
            macro.instance_variable_set(:@name, id)
            macro
          else
            raise "Types are not supported in policy files" if type

            policy = Policy.allocate
            # TODO: set all ivars to nil in correct order
            policy.instance_variable_set(:@start, pos)
            policy.type = id
            policy.rules = []
            policy
          end
        when Policy, Macro, Rule
          case type
          when "var"
            rule = Rule::VarAssignment.allocate
            rule.instance_variable_set(:@start, pos)
            rule.instance_variable_set(:@end, nil)
            rule.instance_variable_set(:@name, id)
            rule.instance_variable_set(:@rules, [])
            return rule
          when "call"
            rule = Rule::MacroCall.allocate
            rule.instance_variable_set(:@start, pos)
            rule.instance_variable_set(:@end, nil)
            rule.instance_variable_set(:@name, id)
            rule.instance_variable_set(:@props, {})
            return rule
          when "op"
            rule = Rule::Op.allocate
            rule.instance_variable_set(:@start, pos)
            rule.instance_variable_set(:@end, nil)
            rule.instance_variable_set(:@name, id)
            rule.instance_variable_set(:@rules, [])
            rule.instance_variable_set(:@args, [])
            return rule
          when "oneof"
            # TODO: assert id = "_"
            rule = Rule::OneOf.allocate
            rule.instance_variable_set(:@start, pos)
            rule.instance_variable_set(:@end, nil)
            rule.instance_variable_set(:@rules, [])
            return rule
          end
          raise "current_node is #{current_node.inspect} type=#{type.inspect} id=#{id.inspect}" if type

          case id
          when /\A\$/
            builtin = ::Regexp.last_match.post_match

            rule = Rule::Error.allocate
            rule.instance_variable_set(:@start, pos)
            rule.instance_variable_set(:@end, nil)
            rule.instance_variable_set(:@message, builtin)
          when /\A%/
            rule = Rule::MacroCall.allocate
            rule.instance_variable_set(:@start, pos)
            rule.instance_variable_set(:@end, nil)
            rule.instance_variable_set(:@name, ::Regexp.last_match.post_match)
            rule.instance_variable_set(:@props, {})
          else
            rule = Rule::PropertyValue.allocate
            rule.instance_variable_set(:@start, pos)
            rule.instance_variable_set(:@end, nil)
            rule.instance_variable_set(:@name, id)
            rule.instance_variable_set(:@rules, [])
          end
          rule

        else
          raise "child #{id.inspect} of #{current_node.inspect}"
        end
      end

      def terminate_node(scanner, closing_needed: false)
        return unless (node = super)

        # validate types of all ivars

        node.freeze
      end

      def node_end(node, pos)
        return if node.is_a?(String)

        node.end = pos
      end

      def parse
        @policy_set = PolicySet.new(source: @scanner.string, filename: @filename)
        super
        @policy_set
      end

      def current_node
        @nodes.last
      end

      def add_property(name, value)
        case n = current_node
        when Policy, Rule::Error
          # ignore
        when Macro
          n.props[name] = Macro::Type[value]
        when Rule::MacroCall
          n.props[name] = value
        # when Rule::PropertyValue
        #   n.rules << Rule::Op.new(start: n.start, name:, value:)
        else
          raise "#{current_node.inspect} add_property #{name.inspect} #{value.inspect}"
        end
      end

      def add_argument(value)
        case n = current_node
        when String
          @policy_set.description += value
        when Policy
          n.name = value
        when Rule::Error
          # ignore
        when Rule::PropertyValue
          if n.value
            n.rules << Rule::Error.new(start: n.start, message: "Property value already has a value", end: n.end)
          else
            n.value = value
          end
        when Rule::Op
          n.args << value
        else
          raise "#{n.inspect} add_argument #{value.inspect}"
        end
      end

      def add_child(child)
        case n = current_node
        when nil
          case child
          when Policy
            @policy_set.policies << child
          when Macro
            @policy_set.defs[child.name] = child
          when String
            @policy_set.description = child
          else
            raise "#{n.inspect} add_child #{child.inspect}"
          end
        when Rule::Error
          # don't add children to errors
        when Policy, Macro, Rule
          n.rules << child
        else
          raise "#{n.inspect} add_child #{child.inspect}"
        end
      end

      def make_value(type, value, val_start, val_end)
        return Rule::Variable.new(name: value, start: val_start, end: val_end) if type == "var"
        return Rule::PropertyValue.new(name: value, start: val_start, end: val_end) if type == "attr"
        return /\A#{value}\z/ if type == "regex"

        raise "current_node is #{current_node.inspect} type=#{type.inspect} value=#{value.inspect}" if type

        case current_node
        when Rule
          Rule::Lit.new(value:, start: val_start, end: val_end)
        else
          value
        end
      end
    end

    class Policy2Parser < KDLParser
      def parse
        @policy_set = PolicySet.new(source: @scanner.string, filename: @filename)
        super
        @policy_set
      end

      def current_node
        @nodes.last || @policy_set
      end

      def make_node(type, id, pos)
        current_node.accept_child(type, id, pos)
      end

      def add_property(name, value)
        current_node.accept_property(name, value)
      end

      def add_argument(value)
        current_node.accept_argument value
      end

      def node_end(node, pos)
        node&.end = pos
      end
    end
  end
end
