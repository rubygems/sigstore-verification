# frozen_string_literal: true

require_relative "verification/version"
require_relative "verification/policy"
require_relative "verification/kdl_parser"
require_relative "verification/rubygems_plugin"
require "literal"
require "sigstore"
require "sigstore/models"
require "sigstore/verifier"

module Sigstore
  module Verification
    class Error < StandardError; end
    # Your code goes here...

    class Subject < Literal::Struct
      prop :type, String
      prop :name, String
      prop :source, _String?
      prop :source_type, _String?
      prop :version, _Map("major" => Integer, "minor" => Integer, "patch" => Integer, "exact" => String)
      prop :platform, String
      prop :sigstore_bundles, _Array(Sigstore::Bundle::V1::Bundle)
      prop :source_object, _Any
      prop :attestations, _Array(
        _Map(
          "x509" => _Hash(String, _JSONData),
          "messageSignature" => _Boolean?,
          "dsseEnvelope" => _Map?(
            "payload" => _Hash(String, _JSONData),
            "payloadType" => _String
          )
        )
      )

      def self.for_installer(installer)
        spec = installer.spec
        sigstore_bundles = fetch_bundles(spec)
        new(
          type: "rubygem",
          name: installer.spec.name,
          version: {
            "major" => spec.version.segments[0],
            "minor" => spec.version.segments[1],
            "patch" => spec.version.segments[2],
            "exact" => spec.version.to_s
          },
          source: spec.source.to_s,
          platform: spec.platform.to_s,
          attestations: [],
          sigstore_bundles:,
          source_object: spec.source
        )
      end

      def self.fetch_bundles(spec)
        case spec.full_name
        when "sigstore-0.1.1"
          [
            Gem::RemoteFetcher.fetcher.fetch_path(
              "https://github.com/sigstore/sigstore-ruby/releases/download/v0.1.1/sigstore-0.1.1.gem.sigstore.json"
            )
          ]
        else
          []
        end.map! do |bundle|
          Sigstore::Bundle::V1::Bundle.decode_json(bundle, registry: REGISTRY)
        end
      end
    end
  end
end
