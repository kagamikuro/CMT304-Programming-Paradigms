
sub traverseDir{
      my $dir = shift;
      my $numberOfRestored = 0;

      #if this is a file, deal with it and return
      if(-f $dir){
            restoreFile($dir);
            return;
      } 
      #traverse each items on this directory
      opendir(DIR, $dir);
      my @dirs = readdir(DIR);
      foreach $d (@dirs){
            #if this item direct to current directory or parent directory, continue
            if($d eq '.' || $d eq '..'){
                  next;
            }
            $newdir = $dir."/$d";
            #if this item is a directory, recursively execute it
            if(-d $newdir){
                  traverseDir($newdir);
            }else{
                  restoreFile($newdir);
           }
      }
}

sub restoreFile{
      my $file = shift;
      open F, $file  or die "Can't open file '$file'for read. $!";
            my $line = <F>; 
      close F;    
      $_ = $line;
      # if this picture is a gif image
      if(/gif/ ||/Gif/ || /GIF/){
            $new_name ="$file.gif";
            rename $file, $new_name;
            $numberOfRestored ++;
      }
      # if this picture is a png image
      if(/png/ ||/Png/ || /PNG/){ 
            $new_name ="$file.png";
            rename $file, $new_name;
            $numberOfRestored ++;
      }
      # if this picture is a jpg image
      if(/jpeg/ || /JPEG/ || /jfif/ || /JFIF/){ 
            $new_name ="$file.jpg";
            rename $file, $new_name;
            $numberOfRestored ++;
      }
      
      
}

for(my $i=0;$i<=$#ARGV; $i++){
   my $dir = $ARGV[$i];
   traverseDir($dir);
   print "$dir   $numberOfRestored";
}

