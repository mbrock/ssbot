defmodule EInk do
  def eink_latex!(latex) do
    Req.new(
      http_errors: :raise,
      method: :post,
      body: latex,
      headers: [content_type: "text/plain"]
    )
    |> Req.request!(url: "http://urbion/epap/DISPLAY-LATEX")
  end

  def eink!(img) do
    Req.new(
      http_errors: :raise,
      method: :post,
      body: img,
      headers: [content_type: "image/png"]
    )
    |> Req.request!(url: "http://urbion/epap/DISPLAY-IMAGE")
  end

  def latex_preamble do
    ~S"""
    \documentclass[12pt,twocolumn]{extarticle}
    \usepackage[
      paperwidth=209.66mm,paperheight=157.25mm,
      margin=0.8cm,includefoot]{geometry}
    \usepackage[
      width=209.66mm,height=157.25mm,center,frame,noinfo]{crop}
    \usepackage{parskip}
    \usepackage{ebgaramond}
    \usepackage[sc]{titlesec}
    \begin{document}
    """
  end

  def latex_postamble do
    ~S"""
    \end{document}
    """
  end

  def latex(src) do
    """
    #{latex_preamble()}
    #{src}
    #{latex_postamble()}
    """
  end

  def latex_to_png_file(src) do
    Temp.track!()

    dir_path = Temp.mkdir!("nodetown-latex")
    tex_path = Path.join(dir_path, "doc.tex") |> IO.inspect(label: :tex_path)
    File.write!(tex_path, src)

    System.cmd(
      "latex",
      [tex_path],
      cd: dir_path,
      into: IO.stream()
    )

    System.cmd(
      "dvipng",
      ["-D", "226.785", "doc"],
      cd: dir_path,
      into: IO.stream()
    )

    File.read!(Path.join(dir_path, "doc1.png"))
  end

  def org_to_latex(src) do
    Temp.track!()

    org_path = Temp.path!("nodetown.org")
    File.write!(org_path, src)

    case System.cmd("pandoc", ["-f", "markdown", "-t", "latex", org_path]) do
      {org, 0} ->
        {:ok, org}

      {_, status} ->
        {:error, {:pandoc, status}}
    end
  end

  def weather do
    {lat, lon} = {56.9475072, 24.1401856}
    key = Application.fetch_env!(:nodetown, :weather_api_key)
    url = "https://api.pirateweather.net/forecast/#{key}/#{lat},#{lon}?units=si"
    Req.get!(url).body
  end

  def weather_gpt3(lang) do
    weather =
      weather()
      |> Access.get("currently")
      |> Map.take(
        ~w"apparentTemperature humidity icon precipIntensity precipProbability precipType summary temperature"
      )
      |> Map.put("time", to_string(DateTime.now!("Europe/Riga")))

    GPT3.complete!(
      context: NodeTown.NS.AI.Weather,
      model: "text-davinci-003",
      max_tokens: 300,
      temperature: 0,
      prompt: """
      Given weather data, write a brief, informative summary, in #{lang}.
      Write in the present tense.
      Use casual language. Don't use excessive decimals.
      Omit needless words like "currently".
      Mention the current date and time, simplified.

      Weather data:
      #{inspect(weather, pretty: true)}

      Summary (#{lang}, Markdown):
      """
    )
  end

  def language_gpt3(lang) do
    GPT3.complete!(
      context: NodeTown.NS.AI.EInk,
      model: "text-davinci-003",
      max_tokens: 500,
      temperature: 0.5,
      prompt: """
      Write a short paragraph in #{lang} about some interesting topic,
      for someone who is learning #{lang}.

      Level: easy to moderate

      Output (Markdown blockquote, names **bold**):
      """
    )
  end

  def daily_eink() do
    text = """
    #{weather_gpt3("Latvian")}

    #{weather_gpt3("Swedish")}

    #{language_gpt3("Swedish")}
    """

    {:ok, src} = org_to_latex(text)

    src
  end

  defmodule Worker do
    use Oban.Worker, queue: :openai

    @impl Oban.Worker
    def perform(%Oban.Job{}) do
      EInk.daily_eink()
      |> IO.inspect()
      |> EInk.eink_latex!()

      :ok
    end
  end
end
