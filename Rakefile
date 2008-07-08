task :test do
  system "emacs -Q -l #{File.dirname(__FILE__)}/test/init.el"
end

MYDIR  = File.dirname(__FILE__)
DOCDIR = "#{MYDIR}/doc"

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

task :default => :test
