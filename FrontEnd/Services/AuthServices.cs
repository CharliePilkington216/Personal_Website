using System.Net.Http.Json;

internal class AuthServices
{
    private readonly HttpClient _httpClient;
    private readonly AuthState _authState;

    public AuthServices(HttpClient httpClient, AuthState authState)
    {
        _httpClient = httpClient;
        _authState = authState;
    }

    public async Task Login(LogInRequest request)
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Post,
            "https://api.charlespilkington.dev/auth/login"
        );

        httpRequest.Content = JsonContent.Create(request);
        httpRequest.IncludeCookies();

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();

        LogInResponse? result = await response.Content.ReadFromJsonAsync<LogInResponse>();

        if (result is null || string.IsNullOrEmpty(result.Token))
        {
            throw new InvalidOperationException(
                "Login response did not contain an access token."
            );
        }

        _authState.SetToken(result.Token);
    }

    public async Task Refresh()
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Post,
            "https://api.charlespilkington.dev/auth/refresh"
        );

        httpRequest.IncludeCookies();

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();

        LogInResponse? result = await response.Content.ReadFromJsonAsync<LogInResponse>();

        if (result is null || string.IsNullOrEmpty(result.Token))
        {
            throw new InvalidOperationException(
                "Refresh response did not contain an access token."
            );
        }

        _authState.SetToken(result.Token);
    }

    public async Task Logout()
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Post,
            "https://api.charlespilkington.dev/auth/logout"
        );

        httpRequest.IncludeCookies();

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();

        _authState.Clear();
    }
}