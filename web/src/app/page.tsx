import { Navbar } from '@/components/Navbar';
import { NameGeneratorHero } from '@/components/NameGeneratorHero';
import { BenchmarkDashboard } from '@/components/BenchmarkDashboard';
import { LanguageShowcase } from '@/components/LanguageShowcase';
import { QuickstartDocs } from '@/components/QuickstartDocs';
import { Footer } from '@/components/Footer';

export default function Home() {
  return (
    <div className="min-h-screen flex flex-col bg-slate-50 dark:bg-[#090d16] text-slate-900 dark:text-slate-100 transition-colors">
      <Navbar />
      <main className="flex-1">
        <NameGeneratorHero />
        <BenchmarkDashboard />
        <LanguageShowcase />
        <QuickstartDocs />
      </main>
      <Footer />
    </div>
  );
}
