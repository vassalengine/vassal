package VASSAL.tools;

import java.io.File;
import javax.swing.filechooser.FileFilter;

/**
 * A generic by-extension FileFilter.
 *
 * @author uckelman
 */
public class ExtensionFileFilter extends FileFilter {
   private String[] types;
   private String desc;

   /**
    * @param desc The description of this filter.
    * @param types A list of the extensions accepted by this filter.
    */
   public ExtensionFileFilter(String desc, String [] types) {
      super();
      this.desc = desc;
      System.arraycopy(types, 0, this.types, 0, types.length);
   }

   /**
    * @return Whether the given file is accepted by this filter.
    */
   public boolean accept(File f) {
      if (f.isDirectory()) return true;
      String name = f.getName().toLowerCase();
      for (int i = 0; i < types.length; i++) {
        if (name.endsWith(types[i])) return true;
      }
      return false;
   }
 
   /**
    * @return The description of this filter.
    */
   public String getDescription() {
      return desc;
   }
   
   /**
    * A FileFilter for GIF, JPEG, PNG, and SVG images. Used by file choosers
    * to filter out files which aren't images.
    */
   public static class ImageFileFilter extends ExtensionFileFilter {
      public static final String[] types = {
         ".gif", ".jpg", ".jpeg", ".png", ".svg"
      };

      public ImageFileFilter() {
         super("Image files", types);
      }
   }

   /**
    * A FileFilter for AIFF, AU, and WAV files. Used by file choosers to
    * filter out files which aren't audio files.
    */
   public static class AudioFileFilter extends ExtensionFileFilter {
      public static final String[] types = {
         ".aiff", ".au", ".wav"
      };

      public AudioFileFilter() {
         super("Audio files", types);
      }
   }  
}
