
MYDIR  = File.dirname(__FILE__)
DOCDIR = "#{MYDIR}/doc"
TESTDIR = "#{MYDIR}/test"

namespace "test" do
  
  desc "Run tests using `emacs-snapshot'"
  task :snapshot do
    system "emacs-snapshot -Q -l #{TESTDIR}/init.el"
  end

  desc "Run tests using `emacs-22'"
  task :twenty_two do
    system "emacs22 -Q -l #{TESTDIR}/init.el"
  end
  
  desc "Run tests using `emacs'"
  task :emacs do
    system "emacs -Q -l #{TESTDIR}/init.el"
  end
  
end

namespace "doc" do

  desc "Compile the html documentation"
  task :make_html do
    system "makeinfo --html #{DOCDIR}/rinari.texi"
  end

  desc "Compile info documentation"
  task :make_info do
    system "makeinfo #{DOCDIR}/rinari.texi"
  end
  
  desc "Install info documentation"
  task :install_info => :make_info do
    system "sudo install-info #{DOCDIR}/rinari.info"
  end
  
  desc "Remove compiled documentation"
  task :clean do
    system "rm -rf #{DOCDIR}/rinari" if FileTest.exists? "#{DOCDIR}/rinari"
    system "rm #{DOCDIR}/rinari.info" if FileTest.exists? "#{DOCDIR}/rinari.info"
  end

end

task :default => :'test:emacs'
