defmodule NodeTown.Readwise do
  @moduledoc """
  Readwise is a service for collects highlights from your Kindle
  and other sources.

  This module provides a simple interface to the Readwise API.
  """

  @base_url "https://readwise.io/api/v2"

  def export(options) do
    req = Req.get("#{@base_url}/export")
  end
end
