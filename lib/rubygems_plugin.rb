# frozen_string_literal: true

Gem.pre_install do |installer|
  gem "literal"
  gem "sigstore"
  require_relative "sigstore/verification"
  Sigstore::Verification::RubygemsPlugin.pre_install(installer)
rescue LoadError => e
  msg = "sigstore-verification is not installed, skipping verification for #{installer.spec.name}\n\n#{e.full_message}"
  question = "Do you want to proceed with installation without verification?"
  if defined?(Bundler)
    Bundler.ui.warn(msg)
    Bundler.ui.yes?(question)
  else
    Gem::DefaultUserInteraction.ui.alert_warning(msg, question)
    false
  end
end
