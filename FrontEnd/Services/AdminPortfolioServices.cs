using System.Net.Http.Json;

internal class AdminPortfolioServices
{
    private readonly HttpClient _httpClient;
    private readonly AuthState _authState;

    public AdminPortfolioServices(HttpClient httpClient, AuthState authState)
    {
        _httpClient = httpClient;
        _authState = authState;
    }

    public async Task<List<ProjectWithId>> GetProjectsWithId()
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Get,
            "http://localhost:8080/portfolio/admin/projects"
        );

        httpRequest.AddAuthentication(_authState);

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();

        return await response.Content.ReadFromJsonAsync<List<ProjectWithId>>() ?? [];
    }

    public async Task<ProjectWithId> PostProject(Project project)
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Post,
            "http://localhost:8080/portfolio/admin/projects"
        );

        httpRequest.AddAuthentication(_authState);
        httpRequest.Content = JsonContent.Create(project);

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();

        return await response.Content.ReadFromJsonAsync<ProjectWithId>() ?? 
                throw new InvalidOperationException("Project Post request returned empty");
    }

    public async Task<ProjectWithId> PutProject(string projectId, Project project)
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Put,
            $"http://localhost:8080/portfolio/admin/projects/{projectId}"
        );

        httpRequest.AddAuthentication(_authState);
        httpRequest.Content = JsonContent.Create(project);

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();

        return await response.Content.ReadFromJsonAsync<ProjectWithId>() ?? 
                throw new InvalidOperationException("Project Put request returned empty");
    }

    public async Task DeleteProject(string projectId)
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Delete,
            $"http://localhost:8080/portfolio/admin/projects/{projectId}"
        );

        httpRequest.AddAuthentication(_authState);

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();
    }

    public async Task<List<TagWithId>> GetTagsWithId()
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Get,
            "http://localhost:8080/portfolio/admin/tags"
        );

        httpRequest.AddAuthentication(_authState);

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();

        return await response.Content.ReadFromJsonAsync<List<TagWithId>>() ?? [];
    }

    public async Task<TagWithId> PostTag(Tag tag)
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Post,
            "http://localhost:8080/portfolio/admin/tags"
        );

        httpRequest.AddAuthentication(_authState);
        httpRequest.Content = JsonContent.Create(tag);

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();

        return await response.Content.ReadFromJsonAsync<TagWithId>() ?? 
                throw new InvalidOperationException("Tag Post request returned empty");
    }

    public async Task DeleteTag(string tagId)
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Delete,
                $"http://localhost:8080/portfolio/admin/tags/{tagId}"
        );

        httpRequest.AddAuthentication(_authState);

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();
    }
}