# frozen_string_literal: true

module KDL
  class Node
    def deconstruct
      [name, arguments, properties, children]
    end

    def deconstruct_keys(_keys)
      { name:, arguments:, properties:, children: }
    end

    def attributes_for_super_diff
      { name:, type:, arguments:, properties:, children: }
    end
  end
end

module KDL
  class Value
    def deconstruct
      [value, format, type]
    end

    def deconstruct_keys(_keys)
      { value:, format:, type: }
    end

    def attributes_for_super_diff
      { value:, format:, type: }
    end
  end
end

KDL_TEST_FILES = %w[
  all_escapes.kdl
  all_node_fields.kdl
  arg_and_prop_same_name.kdl
  arg_false_type.kdl
  arg_float_type.kdl
  arg_hex_type.kdl
  arg_null_type.kdl
  arg_raw_string_type.kdl
  arg_string_type.kdl
  arg_true_type.kdl
  arg_type.kdl
  arg_zero_type.kdl
  asterisk_in_block_comment.kdl
  backslash_in_bare_id.kdl
  bare_arg.kdl
  bare_emoji.kdl
  binary.kdl
  binary_trailing_underscore.kdl
  binary_underscore.kdl
  blank_arg_type.kdl
  blank_node_type.kdl
  blank_prop_type.kdl
  block_comment.kdl
  block_comment_after_node.kdl
  block_comment_before_node.kdl
  block_comment_before_node_no_space.kdl
  block_comment_newline.kdl
  boolean_arg.kdl
  boolean_prop.kdl
  brackets_in_bare_id.kdl
  chevrons_in_bare_id.kdl
  comma_in_bare_id.kdl
  comment_after_arg_type.kdl
  comment_after_node_type.kdl
  comment_after_prop_type.kdl
  comment_in_arg_type.kdl
  comment_in_node_type.kdl
  comment_in_prop_type.kdl
  commented_arg.kdl
  commented_child.kdl
  commented_line.kdl
  commented_node.kdl
  commented_prop.kdl
  crlf_between_nodes.kdl
  dash_dash.kdl
  dot_but_no_fraction.kdl
  dot_but_no_fraction_before_exponent.kdl
  dot_in_exponent.kdl
  dot_zero.kdl
  emoji.kdl
  empty.kdl
  empty_arg_type.kdl
  empty_child.kdl
  empty_child_different_lines.kdl
  empty_child_same_line.kdl
  empty_child_whitespace.kdl
  empty_node_type.kdl
  empty_prop_type.kdl
  empty_quoted_node_id.kdl
  empty_quoted_prop_key.kdl
  empty_string_arg.kdl
  esc_newline_in_string.kdl
  esc_unicode_in_string.kdl
  escline.kdl
  escline_comment_node.kdl
  escline_line_comment.kdl
  escline_node.kdl
  false_prefix_in_bare_id.kdl
  false_prefix_in_prop_key.kdl
  false_prop_key.kdl
  hex.kdl
  hex_int.kdl
  hex_int_underscores.kdl
  hex_leading_zero.kdl
  illegal_char_in_binary.kdl
  illegal_char_in_hex.kdl
  illegal_char_in_octal.kdl
  int_multiple_underscore.kdl
  just_block_comment.kdl
  just_child.kdl
  just_newline.kdl
  just_node_id.kdl
  just_space.kdl
  just_space_in_arg_type.kdl
  just_space_in_node_type.kdl
  just_space_in_prop_type.kdl
  just_type_no_arg.kdl
  just_type_no_node_id.kdl
  just_type_no_prop.kdl
  leading_newline.kdl
  leading_zero_binary.kdl
  leading_zero_int.kdl
  leading_zero_oct.kdl
  multiline_comment.kdl
  multiline_nodes.kdl
  multiline_string.kdl
  multiple_dots_in_float.kdl
  multiple_dots_in_float_before_exponent.kdl
  multiple_es_in_float.kdl
  multiple_x_in_hex.kdl
  negative_exponent.kdl
  negative_float.kdl
  negative_int.kdl
  nested_block_comment.kdl
  nested_children.kdl
  nested_comments.kdl
  nested_multiline_block_comment.kdl
  newline_between_nodes.kdl
  newlines_in_block_comment.kdl
  no_decimal_exponent.kdl
  no_digits_in_hex.kdl
  node_false.kdl
  node_true.kdl
  node_type.kdl
  null_arg.kdl
  null_prefix_in_bare_id.kdl
  null_prefix_in_prop_key.kdl
  null_prop.kdl
  null_prop_key.kdl
  numeric_arg.kdl
  numeric_prop.kdl
  octal.kdl
  only_cr.kdl
  only_line_comment.kdl
  only_line_comment_crlf.kdl
  only_line_comment_newline.kdl
  parens_in_bare_id.kdl
  parse_all_arg_types.kdl
  positive_exponent.kdl
  positive_int.kdl
  preserve_duplicate_nodes.kdl
  preserve_node_order.kdl
  prop_false_type.kdl
  prop_float_type.kdl
  prop_hex_type.kdl
  prop_null_type.kdl
  prop_raw_string_type.kdl
  prop_string_type.kdl
  prop_true_type.kdl
  prop_type.kdl
  prop_zero_type.kdl
  question_mark_at_start_of_int.kdl
  question_mark_before_number.kdl
  quote_in_bare_id.kdl
  quoted_arg_type.kdl
  quoted_node_name.kdl
  quoted_node_type.kdl
  quoted_numeric.kdl
  quoted_prop_name.kdl
  quoted_prop_type.kdl
  r_node.kdl
  raw_arg_type.kdl
  raw_node_name.kdl
  raw_node_type.kdl
  raw_prop_type.kdl
  raw_string_arg.kdl
  raw_string_backslash.kdl
  raw_string_hash_no_esc.kdl
  raw_string_just_backslash.kdl
  raw_string_just_quote.kdl
  raw_string_multiple_hash.kdl
  raw_string_newline.kdl
  raw_string_prop.kdl
  raw_string_quote.kdl
  repeated_arg.kdl
  repeated_prop.kdl
  same_args.kdl
  same_name_nodes.kdl
  sci_notation_large.kdl
  sci_notation_small.kdl
  semicolon_after_child.kdl
  semicolon_in_child.kdl
  semicolon_separated.kdl
  semicolon_separated_nodes.kdl
  semicolon_terminated.kdl
  single_arg.kdl
  single_prop.kdl
  slash_in_bare_id.kdl
  slashdash_arg_after_newline_esc.kdl
  slashdash_arg_before_newline_esc.kdl
  slashdash_child.kdl
  slashdash_empty_child.kdl
  slashdash_full_node.kdl
  slashdash_in_slashdash.kdl
  slashdash_negative_number.kdl
  slashdash_node_in_child.kdl
  slashdash_node_with_child.kdl
  slashdash_only_node.kdl
  slashdash_only_node_with_space.kdl
  slashdash_prop.kdl
  slashdash_raw_prop_key.kdl
  slashdash_repeated_prop.kdl
  space_after_arg_type.kdl
  space_after_node_type.kdl
  space_after_prop_type.kdl
  space_in_arg_type.kdl
  space_in_node_type.kdl
  space_in_prop_type.kdl
  square_bracket_in_bare_id.kdl
  string_arg.kdl
  string_prop.kdl
  tab_space.kdl
  trailing_crlf.kdl
  trailing_underscore_hex.kdl
  trailing_underscore_octal.kdl
  true_prefix_in_bare_id.kdl
  true_prefix_in_prop_key.kdl
  true_prop_key.kdl
  two_nodes.kdl
  type_before_prop_key.kdl
  unbalanced_raw_hashes.kdl
  underscore_at_start_of_fraction.kdl
  underscore_at_start_of_hex.kdl
  underscore_at_start_of_int.kdl
  underscore_before_number.kdl
  underscore_in_exponent.kdl
  underscore_in_float.kdl
  underscore_in_fraction.kdl
  underscore_in_int.kdl
  underscore_in_octal.kdl
  unusual_bare_id_chars_in_quoted_id.kdl
  unusual_chars_in_bare_id.kdl
  zero_arg.kdl
  zero_float.kdl
  zero_int.kdl

  unclosed_node.kdl
].freeze

RSpec.describe Sigstore::Verification do
  it "has a version number" do
    expect(Sigstore::Verification::VERSION).not_to be_nil
  end

  [Sigstore::Verification::KDLParser, Sigstore::Verification::KDLNodeParser].each do |parser|
    describe parser do
      shared_context "when parsing KDL" do |root, name|
        subject(:formatted) do
          str = described_class.format_ast(ast)
          if parser == Sigstore::Verification::KDLNodeParser
            str.gsub!(/(\d+)\.(\d+)?[eE]/) do
              "#{Regexp.last_match(1)}.#{Regexp.last_match(2) || "0"}E"
            end
          end
          str
        end

        let(:input) { File.read(File.join(root, "input", name)) }
        let(:expected_file) { File.join(root, "expected_kdl", name) }
        let(:expected) { File.read(expected_file) }
        let(:ast) { described_class.parse(input) }

        if parser == Sigstore::Verification::KDLNodeParser
          it "matches KDL.parse_document" do
            expect(ast).to eq(KDL.parse_document(input)) if File.exist?(expected_file)
          end
        end

        it "parses or errors" do
          if File.exist?(expected_file)
            expect(formatted).to eq(expected),
                                 "#{input}\n\n#{expected.inspect}\n#{formatted.inspect}\n#{ast.pretty_inspect}"
          else
            expect { formatted } # rubocop:disable RSpec/RepeatedSubjectCall
              .to raise_error(RuntimeError), "should fail to parse\n#{input}\n#{begin
                ast.pretty_inspect
              rescue StandardError => e
                e.full_message
              end}"
          end
        end
      end

      KDL_TEST_FILES.each do |name|
        next if name == "no_decimal_exponent.kdl"

        context "with /Users/segiddins/Development/github.com/kdl-org/kdl/tests/test_cases/input/#{name}" do
          include_examples "when parsing KDL", "/Users/segiddins/Development/github.com/kdl-org/kdl/tests/test_cases",
                           name
        end
      end
    end
  end

  if false
    [
      "rubygem \"sigstore\" { version.major 0; version.minor 1; version.patch 2; }",
      "rubygem \"sigstore\" { version.major 0; version.minor 1; }",
      "rubygem \"sigstore\" { version.major 0; }",
      "rubygem \"sigstore\" { version.major 0; version.minor 1; version.patch 2; version { exact \"0.1.2\"; }; }",
      "_{}_",
      "_{}",
      "_{_}",
      "_{_;};",
      "_ \"",
      "true",
      "false",
      "null",
      "_ 0b",
      "0b",
      "_ 0b8",
      "_ 0b_",
      "0x",
      "_ true=true",
      'r#"true" "true"=true',
      "_{}/**/",
      "_{}/**/;",
      "_{};/**/",
      "_{};/**/;",
      "_{} z",
      "_ \"a",
      '_ "a\e"',
      '_ "\\',
      "_ -0b",
      "_ +0b",
      "_ 0b",
      "_ -0b1",
      "_ +0b1",
      "_ 0b1",
      "_ 0b17",
      "_ 0b7",
      "_ 0O0",
      "_ {\n\n{\\n}\n}",
      "_ 011",
      "//",
      "//\n",
      "/-",
      "/--",
      "/-\n",
      File.read("/Users/segiddins/Development/github.com/rubygems/sigstore-verification/sigstore-policy.kdl")
    ].each do |str|
      it "matches the KDL parser output on #{str.pretty_inspect}" do
        error = nil
        begin
          expected = KDL.parse_document(str, parse_types: false)
        rescue Racc::ParseError, KDL::Tokenizer::Error => e
          error = e
        else
          actual = Sigstore::Verification::KDLNodeParser.parse(str)
          expect(actual).to eq(expected)
        end
        next unless error

        loc = error.message.match(/ \((\d+:\d+)\)(?= |\z)/)&.[](1)
        expect do
          Sigstore::Verification::KDLNodeParser.parse(str)
        end.to raise_error(RuntimeError, a_string_including(loc).or(a_string_including("")))
      end
    end
  end

  shared_examples "parses KDL" do |test|
    describe test["name"] do
      if test["syntax_tree"]
        it "parses" do
          actual = Sigstore::Verification::KDLNodeParser.parse(test["input"])
          syntax_tree = test["syntax_tree"]
          test["syntax_tree"] = actual
          expect(actual).to eq(syntax_tree)
        end

        it "matches KDL.parse_document" do
          expect(KDL.parse_document(test["input"])).to eq(test["syntax_tree"])
        end

        it "matches the Rust parser" do
          require "open3"
          output, err, status = Open3.capture3("./rs/target/release/rs", { stdin_data: test["input"] })
          expect(status).to be_success, err

          parsed = begin
            YAML.load(output, permitted_classes: [KDL::Document, KDL::Node,
                                                  KDL::Value::String])
          rescue StandardError => e
            raise "#{output}\n\n#{e.full_message}"
          end
          expect(parsed).to eq(test["syntax_tree"])
        end
      end

      unless test.key?("syntax_tree")
        it "raises a Racc::ParseError" do
          error = test["racc_error"]
          expect { KDL.parse_document(test["input"]) }
            .to raise_error(Racc::ParseError, error || "\n") do |e|
              test["racc_error"] = e.message
            end
        end

        it "raises a RuntimeError" do
          expect { Sigstore::Verification::KDLNodeParser.parse(test["input"]) }
            .to raise_error(RuntimeError, test["error"] || "\n") do |e|
              test["error"] = e.message
            end
        end

        it "has a Rust error" do
          require "open3"
          output, err, status = Open3.capture3("./rs/target/release/rs", { stdin_data: test["input"] })
          expect(status).not_to be_success, output
          expect(err.chomp).to eq(test["rust_error"] || "\n") do |e|
            test["rust_error"] = e.message
          end
        end
      end
    end
  end

  if false
    File.read("parse_tests.yml").then do |input|
      require "yaml"
      tests = YAML.safe_load(input, permitted_classes: [KDL::Document, KDL::Node, KDL::Value::String])
      tests.each do |test|
        include_examples "parses KDL", test
      end
      File.write("parse_tests.yml", YAML.dump(tests)) if ENV["UPDATE_TESTS"]
    end
  end

  File.read("/Users/segiddins/Development/github.com/sigstore/sigstore-ruby/policy-tests.kdl").then do |input|
    doc = KDL.parse_document(input)
    doc.nodes.each do |node|
      node => ["test", [KDL::Value::String[name, _, _]], {}, body]
      it "parses #{name}" do
        policy_set = document = nil

        body.each do |child|
          case child
          in ["document", [], {}, nodes]
            document = KDL::Document.new(nodes)
            policy_set = Sigstore::Verification::PolicyParser.parse(document.to_s)
          in ["ast", [KDL::Value::String[ast, _, _]], {}, _]
            # expect(policy_set.to_ast).to eq(eval(ast))
          in ["example", [], {}, body]
            subject = {}
            match = nil

            body.each do |rule|
              case rule
              in ["subject", [KDL::Value::String[json, _, nil]], {}, []]
                subject = JSON.parse(json)
              in ["match", [KDL::Value::Boolean[match, _, _]], {}, determining_rules]
                # noop
              end
            end

            actual = policy_set.evaluate(subject)
            if match
              expect(actual).to be_success, actual.format_failures
              expect(actual.format_failures).to be_empty
            else
              expect(actual).not_to be_success
            end
          end
        end
      end
    end
  end
end
