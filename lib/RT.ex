defmodule RT do
  require Record

  files = ["res.hrl", "pi.hrl"]

  hrl_files =
    Enum.filter(files, fn f ->
      !String.contains?(f, "/_") and Path.extname(f) == ".hrl"
    end)

  Enum.each(
    hrl_files,
    fn t ->
      Enum.each(
        Record.extract_all(from_lib: "rt/include/" <> t),
        fn {name, definition} ->
          prev = :application.get_env(:rt, :nitro_tables, [])

          case :lists.member(name, prev) do
            true ->
              :skip

            false ->
              Record.defrecord(name, definition)
              :application.set_env(:rt, :nitro_tables, [name | prev])
          end
        end
      )
    end
  )

end
