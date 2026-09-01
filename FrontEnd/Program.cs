using Microsoft.AspNetCore.Components.Web;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using FrontEnd;

var builder = WebAssemblyHostBuilder.CreateDefault(args);
builder.RootComponents.Add<App>("#app");
builder.RootComponents.Add<HeadOutlet>("head::after");

builder.Services.AddScoped(sp => new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });

builder.Services.AddScoped<AdminPortfolioServices>();
builder.Services.AddScoped<AuthServices>();
builder.Services.AddScoped<PortfolioServices>();
builder.Services.AddScoped<TutoringServices>();

builder.Services.AddSingleton<AuthState>();
builder.Services.AddSingleton<PortfolioCache>();

await builder.Build().RunAsync();
