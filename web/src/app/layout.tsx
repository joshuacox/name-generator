import type { Metadata } from "next";
import "./globals.css";

export const metadata: Metadata = {
  title: "Name Generator — Multi-Language Deathmatch & Benchmarks",
  description:
    "An epic multi-language showdown featuring 30+ implementations of a random server name generator. Interactive generator, live benchmarks, and deathmatch leaderboards.",
  keywords: [
    "name generator",
    "benchmarks",
    "hyperfine",
    "zig",
    "nim",
    "crystal",
    "fortran",
    "ada",
    "odin",
    "vlang",
    "rust",
    "go",
    "awk",
  ],
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en" suppressHydrationWarning>
      <head>
        <script
          dangerouslySetInnerHTML={{
            __html: `
              try {
                const storedTheme = localStorage.getItem('theme');
                if (storedTheme) {
                  document.documentElement.setAttribute('data-theme', storedTheme);
                } else if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
                  document.documentElement.setAttribute('data-theme', 'dark');
                } else {
                  document.documentElement.setAttribute('data-theme', 'light');
                }
              } catch (_) {}
            `,
          }}
        />
      </head>
      <body className="antialiased selection:bg-orange-500 selection:text-white">
        {children}
      </body>
    </html>
  );
}
