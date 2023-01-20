defmodule NodeTownWeb.Router do
  use NodeTownWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {NodeTownWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", NodeTownWeb do
    pipe_through :browser

    get "/", PageController, :home

    live "/scrape_items", ItemLive.Index, :index
    live "/scrape_items/new", ItemLive.Index, :new
    live "/scrape_items/:id/edit", ItemLive.Index, :edit

    live "/scrape_items/:id", ItemLive.Show, :show
    live "/scrape_items/:id/show/edit", ItemLive.Show, :edit

    live "/nodes", NodeLive.Index, :index
    live "/nodes/new", NodeLive.Index, :new
    live "/nodes/:id/edit", NodeLive.Index, :edit

    live "/nodes/:id", NodeLive.Show, :show
    live "/nodes/:id/show/edit", NodeLive.Show, :edit

    live "/edges", EdgeLive.Index, :index
    live "/edges/new", EdgeLive.Index, :new
    live "/edges/:id/edit", EdgeLive.Index, :edit

    live "/edges/:id", EdgeLive.Show, :show
    live "/edges/:id/show/edit", EdgeLive.Show, :edit
  end

  # Other scopes may use custom stacks.
  # scope "/api", NodeTownWeb do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:nodetown, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: NodeTownWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
