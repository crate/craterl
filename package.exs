defmodule Craterl.Mixfile do
    use Mix.Project

    def project do
        [
            app: :craterl,
            version: "0.2.0",
            description: description,
            package: package,
            deps: deps,
            language: :erlang
        ]
    end

    defp deps do
        [
            {:lager,   "2.0.3", github: "basho/lager",          tag: "2.0.3"},
            {:jsx,     "2.1.1", github: "talentdeficit/jsx",    tag: "v2.1.1"},
            {:hackney, "1.0.5", github: "benoitc/hackney",      tag: "1.0.5"},
            {:meck,    "0.8.2", github: "eproxus/meck",         branch: "master"}
        ]
    end

    defp description do
        """
        Erlang/Elixir client for crate using the HTTP REST API of crate to
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
