module Literal::Properties
  def kdl_builder(cls)
    Class.new(Literal::Struct) do
      const_set(:BASE, cls)
      mandatory_positional_properties = []

      cls.literal_properties.each do |prop|
        default = prop.default
        type = prop.type
        if prop.required?
          if type === {}
            default = -> { {} }
          elsif type.is_a? Literal::Types::MapType
            default = -> { {} }
            type = Hash
          elsif type === []
            default = -> { [] }
          else
            type = _Nilable(type)
          end
        end
        mandatory_positional_properties << prop.name if prop.positional?
        self.prop(prop.name, type, default:)
      end
      mandatory_positional_properties.freeze

      define_method(:mandatory_positional_properties) do
        mandatory_positional_properties
      end

      def after_initialize
        @value_index = 0
      end

      def accept_value(value)
        self[mandatory_positional_properties[@value_index].name] = value
        @value_index += 1
      end

      def build
        args = mandatory_positional_properties.map do |prop|
          self[prop]
        end
        self.class::BASE.new(
          *args,
          **to_h.delete_if { |key, _| mandatory_positional_properties.include?(key) }
        ).freeze
      end
    end
  end
end

class Foo < Literal::Struct
  prop :a, String
  prop :b, _Nilable(String)
  prop :c, _Array(String)
  prop :d, _Hash(String, String)
  prop :e, _Map("key" => Symbol)
  Builder = kdl_builder(self)
end

class Bar < Literal::Struct
  prop :a, String, :positional
  prop :b, String
  Builder = kdl_builder(self)
end

RSpec.describe "kdl_builder" do
  it "builds a builder" do
    builder = Foo::Builder.new
    builder.a = "a"
    builder.d["key"] = "value"
    builder.e["key"] = :value
    expect(builder.build).to eq(
      Foo.new(
        a: "a", b: nil, c: [], d: { "key" => "value" }, e: { "key" => :value }
      )
    )

    builder = Bar::Builder.new
    builder.a = "a"
    builder.b = "b"
    builder.build
  end
end

RSpec.describe Sigstore::Verification::Policy2Parser do
  subject(:parser) { described_class.new(source) }

  context "with empty source" do
    let(:source) { "" }

    it "parses" do
      expect(parser.parse).to eq(Sigstore::Verification::PolicySet.new(source:))
    end
  end

  context "with a description" do
    let(:source) { "description \"foo\"\nd\n" }

    it "parses" do
      expect(parser.parse).to eq \
        Sigstore::Verification::PolicySet.new(
          source:,
          description: "foo",
          policies: [
            Sigstore::Verification::Policy.new(
              start: 18, type: "d", name: "UGH", end: 19
            )
          ]
        )
    end
  end
end
