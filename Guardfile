guard :shell, :all_after_pass => true do
  watch(%r{.*\.cabal$}) { run_all_tests }
  watch(%r{test/SpecHelper.hs$}) { run_all_tests }

  def run_all_tests
    system("cabal configure && cabal build && cabal test")
  end

  def run_tests(mod)
    specfile = "test/#{mod}Spec.hs"

    if File.exists?(specfile)
      files = [specfile]
    else
      files = Dir['test/**/*.hs']
    end

    if package_db = Dir[".cabal-sandbox/*packages.conf.d", "cabal-dev/*packages.conf.d"].first
      package_db_flag = "-package-db #{package_db}"
    end

    system("ghc -isrc -itest #{package_db_flag} -e main #{files.join(' ')}")
  end

  watch(%r{src/(.+)\.hs$}) { |m| run_tests(m[1]) }
  watch(%r{test/(.+)Spec\.hs$}) { |m| run_tests(m[1]) }
end
