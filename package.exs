defmodule Craterl.Mixfile do
    use Mix.Project

    def project do
        [
            app: :craterl,
            version: "0.2.1",
            description: description,
            package: package,
            deps: deps(),
            language: :erlang
        ]
    end

    defp deps do
        [
            {:jsx,     "~> 2.4.0"},
            {:hackney, "~> 1.0.2"}
        ]
    end

    defp description do
        """
        Erlang client for crate using the HTTP REST API of crate to
        issue SQL queries and manage blobs.
        """
    end

    defp package do
        [
            files: ~w(src include rebar.config rebar.config.script README.md LICENSE CHANGES.txt),
            contributors: ["Peter Sabaini", "Matthias Wahl"],
            licenses: ["Apache 2.0"],
            links: %{"GitHub" => "https://github.com/crate/craterl"}
        ]
    end
end
