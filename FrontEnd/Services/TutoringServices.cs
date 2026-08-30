using System.Net.Http.Json;

internal class TutoringServices
{
    private readonly HttpClient _httpClient;

    public TutoringServices(HttpClient httpClient)
    {
        _httpClient = httpClient;
    }

    public async Task PostTutoringRequest(TutoringRequest tutoringRequest)
    {
        HttpRequestMessage httpRequest = new HttpRequestMessage(
            HttpMethod.Post,
            "http://localhost:8080/tutoring/inquiries"
        );

        httpRequest.Content = JsonContent.Create(tutoringRequest);

        HttpResponseMessage response = await _httpClient.SendAsync(httpRequest);
        response.EnsureSuccessStatusCode();
    }
}