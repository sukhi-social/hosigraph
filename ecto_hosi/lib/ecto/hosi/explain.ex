defmodule Ecto.Hosi.Explain do
  @moduledoc """
  `Repo.explain/1`: what an Ecto query does on hosigraph, without running it.

      MyApp.Repo.explain(from n in MyApp.Note, where: n.content == "hi")
      #=> %{
      #     datalog: [%{q: "[:find ?e :in $ ?p0 :where [?e :ap/type :note] [?e :content ?p0]]",
      #                 inputs: [[:scalar, "hi"]]}],
      #     elixir: ["reads the matching records to build the result"],
      #     warnings: []
      #   }

    * `:datalog` – the requests asked first, as `Repo.datalog/2` takes them (none for a query by
      id). A join that is read afterwards is described under `:elixir`.
    * `:elixir` – what is done on the records after they are read
    * `:warnings` – where every record of a view is read, which costs more as the data grows
  """

  alias Ecto.Hosi.{Hybrid, Reader, RepoApi}

  @doc false
  def explain(repo, queryable, _opts \\ []) do
    meta = RepoApi.meta(repo)

    {{:nocache, {:all, query}}, params} =
      Ecto.Adapter.Queryable.prepare_query(:all, repo.get_dynamic_repo(), queryable)

    Reader.no_subquery!(meta, query)

    case Reader.plan(meta, query, params) do
      {:one_view, view, source, fields, keys, excluded} ->
        plan = one_view(view, source, fields, keys, query)

        if excluded == [] do
          plan
        else
          checked = "leaves out the records of the long not-in list(s) over #{Enum.map_join(excluded, ", ", &inspect/1)}"
          %{plan | elixir: [checked | plan.elixir]}
        end

      :hybrid ->
        hybrid(Hybrid.plan(meta, query, Ecto.Hosi.Types.normalize_params(params)), query)
    end
  end

  defp one_view(view, source, fields, keys, query) do
    {datalog, found} =
      case source do
        {:ids, ids} -> {[], "reads the record(s) #{inspect(ids)} by id and checks the view shows them"}
        {:datalog, request} -> {[request], "reads the matching records to build the result"}
      end

    reads? = Enum.any?(fields, &match?({:field, f} when f != :id, &1))
    count? = fields == [:count]
    sorted? = Enum.any?(keys, fn {_, f} -> f != :id end)

    elixir =
      [
        if(reads? or match?({:ids, _}, source), do: found),
        if(sorted?, do: "sorts the records (order_by)"),
        if(query.limit || query.offset, do: "applies offset and limit"),
        if(count?, do: "counts the matching ids")
      ]
      |> Enum.reject(&is_nil/1)

    unrestricted =
      if query.wheres == [] do
        "reads every record #{inspect(view.module)} shows (the query has no where)"
      end

    paged =
      if query.limit && match?({:datalog, _}, source) && query.wheres != [] do
        "limit is applied after every matching record is found; Repo.highest or Repo.highest_in " <>
          "pages newest-first without that"
      end

    paged =
      if unrestricted && query.limit,
        do: "limit does not narrow what is read; Repo.highest or Repo.highest_in does",
        else: paged

    %{datalog: datalog, elixir: elixir, warnings: Enum.reject([unrestricted, paged], &is_nil/1)}
  end

  defp hybrid(plan, query) do
    first = Enum.map_join(plan.first, ", ", &inspect(&1.module))

    joins =
      for {view, qual, field} <- plan.joins do
        case field do
          nil -> "#{qual} join: reads every record #{inspect(view.module)} shows, and joins here"
          field -> "#{qual} join: reads the #{inspect(view.module)} records by #{field}, and joins here"
        end
      end

    elixir =
      [
        "reads the records Datalog finds, for #{first}"
        | joins
      ] ++
        Enum.reject(
          [
            if(plan.where != nil or plan.conjuncts > 0, do: "checks the whole where again, as SQL means it"),
            if(plan.pushed < plan.conjuncts,
              do: "#{plan.conjuncts - plan.pushed} of #{plan.conjuncts} condition(s) are only checked here"
            ),
            if(query.group_bys != [], do: "group_by"),
            if(query.havings != [], do: "having"),
            if(Enum.any?(query.select.fields, &Ecto.Hosi.Eval.aggregate?/1), do: "aggregates"),
            if(query.distinct, do: "distinct"),
            if(query.order_bys != [], do: "order_by"),
            if(query.limit || query.offset, do: "offset and limit")
          ],
          &is_nil/1
        )

    whole =
      if plan.pushed == 0 do
        "reads every record #{first} shows: no condition of the query goes into Datalog"
      end

    whole_joins =
      for {view, _, nil} <- plan.joins,
          do: "reads every record #{inspect(view.module)} shows (a join with no field to look up by)"

    %{
      datalog: [plan.request],
      elixir: elixir,
      warnings: Enum.reject([whole], &is_nil/1) ++ whole_joins
    }
  end
end
