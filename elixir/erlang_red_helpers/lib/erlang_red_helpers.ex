defmodule ErlangRedHelpers do
  @moduledoc """
  A collction of helpers defined in Elixir but used by the Erlang codebase.
  """

  @doc """
  Convert Markdown content to HTML content.
  """
  def markdown_to_html(content) do
    Earmark.as_html(content)
  end

  def markdown_to_html(content, args) do
    Earmark.as_html(content, args)
  end
end
