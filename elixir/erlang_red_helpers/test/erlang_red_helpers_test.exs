defmodule ErlangRedHelpersTest do
  use ExUnit.Case
  doctest ErlangRedHelpers

  test "greets the world" do
    assert ErlangRedHelpers.markdown_to_html("# hello wolrd") == {:ok, "<h1>\nhello wolrd</h1>\n", []}
  end
end
