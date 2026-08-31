using System.Net.Http.Headers;
using Microsoft.AspNetCore.Components.WebAssembly.Http;

internal static class HttpRequestExtensions
{
    public static void IncludeCookies(this HttpRequestMessage request)
    {
        request.SetBrowserRequestCredentials(
            BrowserRequestCredentials.Include
        );
    }

    public static void AddAuthentication(this HttpRequestMessage request, AuthState authState)
    {
        if (authState.AccessToken is not null)
        {
            request.Headers.Authorization =
                new AuthenticationHeaderValue(
                    "Bearer",
                    authState.AccessToken
                );
        }
    }
}