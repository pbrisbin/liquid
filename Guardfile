def execute(cmd)
  system(cmd) or n 'Build failed', 'hspec', :failed
end

def run_all_tests
  execute %{
    cabal configure --enable-tests &&
    cabal build &&
    cabal test
  }
end

def run_tests(mod)
  specfile = "test/#{mod}Spec.hs"

  if File.exists?(specfile)
    files = [specfile]
  else
    files = Dir['test/**/*.hs']
  end

  execute("ghc -isrc -itest -e main #{files.join(' ')}")
end

guard :shell do
  watch(%r{.*\.cabal$})          { run_all_tests }
  watch(%r{test/SpecHelper.hs$}) { run_all_tests }
  watch(%r{src/(.+)\.hs$})       { |m| run_tests(m[1]) }
  watch(%r{test/(.+)Spec\.hs$})  { |m| run_tests(m[1]) }
end
