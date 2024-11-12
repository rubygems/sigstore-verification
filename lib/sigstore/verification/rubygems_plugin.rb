# frozen_string_literal: true

module Sigstore
  module Verification
    class RubygemsPlugin
      include Gem::UserInteraction

      class BundlerUIProxy
        def initialize(ui)
          @ui = ui
        end

        def say(*args)
          @ui.info(*args)
        end

        def alert_warning(*args)
          @ui.warn(*args)
        end

        def alert_error(statement, question = nil)
          @ui.error(statement)
          return unless question

          *before, after = question.split("\n")
          @ui.send(:tell_me, before.join("\n"))
          @ui.ask(after)
        end

        def verbose(*args)
          @ui.debug(*args)
        end
      end

      MUTEX = Mutex.new

      def self.pre_install(installer)
        MUTEX.synchronize do
          @instance ||= new
          Sigstore.logger = @instance
          if defined?(Bundler)
            bundler_rubygems_ui = Gem::DefaultUserInteraction.ui
            Bundler.rubygems.ui = BundlerUIProxy.new(Bundler.ui)
          end
          @instance.pre_install(installer)
        ensure
          Bundler.rubygems.ui = bundler_rubygems_ui if bundler_rubygems_ui
        end
      end

      module NullPolicy
        def self.verify(_)
          Sigstore::VerificationSuccess.new
        end
      end

      def initialize
        Sigstore.logger = self
        @trust_root = Sigstore::TrustedRoot.production(offline: true)
        @verifier = Sigstore::Verifier.production(trust_root: @trust_root)
      end

      attr_reader :mutex

      def pre_install(installer)
        subject = Subject.for_installer(installer)

        # TODO: make the trust root configurable
        verifier = @verifier

        artifact = Sigstore::Verification::V1::Artifact.new
        if installer.package&.gem.nil?
          alert_warning "No artifact for #{installer.spec.full_name}"
          return
        end

        artifact.artifact = installer.package.gem.with_read_io(&:read)

        verified_attestations = subject.sigstore_bundles.select do |bundle|
          verification_input = Sigstore::Verification::V1::Input.new
          verification_input.artifact = artifact
          verification_input.bundle = bundle
          input = Sigstore::VerificationInput.new(verification_input)
          # TODO: collect unverified attestations and do something with them?
          verifier.verify(input:, policy: NullPolicy, offline: true)
        end

        attestations = []
        verified_attestations.each do |attestation|
          sbundle = Sigstore::SBundle.new(attestation)
          attestations << {
            "x509" => sbundle.leaf_certificate.then do |cert|
              cert.openssl.extensions.each_with_object({}) do |ext, h|
                oid = ext.oid
                value = ext.value

                next unless oid.start_with?("1.3.6.1.4.1.57264.1.") || oid == "subjectAltName"

                if oid.start_with?("1.3.6.1.4.1.57264.1.")
                  f = oid.split(".")[-1].to_i

                  value = OpenSSL::ASN1.decode(ext.value_der) if f >= 8
                  value = value.value if value.is_a?(OpenSSL::ASN1::UTF8String)

                  h[OID_MAP.fetch(oid)] = value
                end

                h[oid] = value
              end
            end,
            "messageSignature" => attestation.message_signature&.then { !!_1 }, # rubocop:disable Style/DoubleNegation
            "dsseEnvelope" => attestation.dsse_envelope&.then do |envelope|
              { "payload" => envelope.payload, "payloadType" => envelope.payload_type }
            end
          }
        end
        subject.attestations = attestations

        # check policy

        # TODO: make the policy location configurable

        filename = ENV.fetch("SIGSTORE_POLICY") { File.expand_path("../../../sigstore-policy.kdl", __dir__) }
        policy_set = Sigstore::Verification::PolicyParser.parse(
          File.read(filename),
          filename:
        )

        digest = OpenSSL::Digest.new("SHA256").update(artifact.artifact).hexdigest

        result = policy_set.evaluate({
                                       "type" => subject.type,
                                       "name" => subject.name,
                                       "version" => subject.version,
                                       "platform" => subject.platform,
                                       "attestation" => subject.attestations,
                                       "digest" => { "sha256" => digest }
                                     })

        success, matching_policies = result.result
        if success
          say "✅ Verified #{installer.spec.full_name}"
        elsif matching_policies.any?
          # TODO: add questions to allow adding a new default policy based on the subject
          alert_error result.format_failures
        else
          suggestion = suggest_policy(subject, digest)

          add_policy = alert_error "No policy found for #{subject.type} #{installer.spec.full_name}",
                                   "Proposed policy:#{suggestion}\n\nWould you like to add it to #{filename}?"
          if add_policy&.match?(/\Ay(es)?\z/i)
            File.open(filename, "a") do |f|
              f.puts suggestion
            end
            say "✅ Added policy for #{installer.spec.full_name}"
            success = true
          end

        end

        success
      end

      def suggest_policy(subject, digest)
        f = StringIO.new
        f.puts "\n\n/* Added automatically by sigstore-verification */"
        if subject.attestations.any? # TODO: test this
          f.puts "rubygem #{subject.name.inspect} {"
          subject.attestations.each do |attestation|
            if attestation.dig("x509", "issuerV2") == "https://token.actions.githubusercontent.com"
              uri = attestation.dig("x509", "buildConfigURI")
              # Extract owner/repo/workflow from URI like:
              # https://github.com/sigstore/sigstore-ruby/.github/workflows/release.yml@refs/tags/v0.1.1
              if (match = uri.match(%r{^https://github\.com/([^/]+)/([^/]+)/\.github/workflows/([^@]+)@}))
                owner, repo, workflow = match.captures
                f.puts "  (call)githubAttestation owner=#{owner.inspect} repository=#{repo.inspect} workflow=#{workflow.inspect}"
                next
              end
            end
            f.puts "  attestation #{attestation.inspect}"
          end
          f.puts "}"
        else
          f.puts "rubygem #{subject.name.inspect} { version.exact #{subject.version["exact"].inspect}; " \
                 "digest.sha256 #{digest.inspect}; }"
        end
        f.string
      end

      def info(&)
        say yield
      end

      def debug(&)
        verbose(&)
      end

      def warn(&)
        alert_warning yield
      end

      def error(&)
        alert_error yield
      end

      OID_MAP = {
        "1.3.6.1.4.1.57264.1.1" => "issuer",
        "1.3.6.1.4.1.57264.1.2" => "githubWorkflowTrigger",
        "1.3.6.1.4.1.57264.1.3" => "githubWorkflowSHA",
        "1.3.6.1.4.1.57264.1.4" => "githubWorkflowName",
        "1.3.6.1.4.1.57264.1.5" => "githubWorkflowRepository",
        "1.3.6.1.4.1.57264.1.6" => "githubWorkflowRef",
        "1.3.6.1.4.1.57264.1.7" => "otherNameSAN",
        "1.3.6.1.4.1.57264.1.8" => "issuerV2",
        "1.3.6.1.4.1.57264.1.9" => "buildSignerURI",
        "1.3.6.1.4.1.57264.1.10" => "buildSignerDigest",
        "1.3.6.1.4.1.57264.1.11" => "runnerEnvironment",
        "1.3.6.1.4.1.57264.1.12" => "sourceRepositoryURI",
        "1.3.6.1.4.1.57264.1.13" => "sourceRepositoryDigest",
        "1.3.6.1.4.1.57264.1.14" => "sourceRepositoryRef",
        "1.3.6.1.4.1.57264.1.15" => "sourceRepositoryIdentifier",
        "1.3.6.1.4.1.57264.1.16" => "sourceRepositoryOwnerURI",
        "1.3.6.1.4.1.57264.1.17" => "sourceRepositoryOwnerIdentifier",
        "1.3.6.1.4.1.57264.1.18" => "buildConfigURI",
        "1.3.6.1.4.1.57264.1.19" => "buildConfigDigest",
        "1.3.6.1.4.1.57264.1.20" => "buildTrigger",
        "1.3.6.1.4.1.57264.1.21" => "runInvocationURI",
        "1.3.6.1.4.1.57264.1.22" => "sourceRepositoryVisibilityAtSigning"
      }.freeze
    end
  end
end
