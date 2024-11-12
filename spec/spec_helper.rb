# frozen_string_literal: true

require "simplecov"
SimpleCov.configure do
  add_filter "spec/"
  enable_coverage :branch
end
SimpleCov.start

require "super_diff/rspec"
require "sigstore/verification"

RSpec.configure do |config|
  # Enable flags like --only-failures and --next-failure
  config.example_status_persistence_file_path = ".rspec_status"

  # Disable RSpec exposing methods globally on `Module` and `main`
  config.disable_monkey_patching!

  config.expect_with :rspec do |c|
    c.syntax = :expect
  end
end

class LiteralEnumInspectionTreeBuilder < SuperDiff::Core::AbstractInspectionTreeBuilder
  def self.applies_to?(value)
    value.is_a?(::Literal::Enum)
  end

  def call
    SuperDiff::Core::InspectionTree.new do |t1|
      t1.add_text(object.name)
    end
  end
end

SuperDiff.configure do |config|
  config.prepend_extra_inspection_tree_builder_classes(LiteralEnumInspectionTreeBuilder)
end
