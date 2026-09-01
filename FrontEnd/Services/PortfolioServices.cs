using System.Net.Http.Json;

internal class PortfolioServices
{
    private readonly HttpClient _httpClient;

    public PortfolioServices(HttpClient httpClient)
    {
        _httpClient = httpClient;
    }

    public async Task<List<Project>> GetProjects()
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Get,
            "https://api.charlespilkington.dev/portfolio/projects"
        );

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();

        return await response.Content.ReadFromJsonAsync<List<Project>>() ?? [];
    }

    public async Task<List<Tag>> GetTags()
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Get,
            "https://api.charlespilkington.dev/portfolio/tags"
        );

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();

        return await response.Content.ReadFromJsonAsync<List<Tag>>() ?? [];
    }
}