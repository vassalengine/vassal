package VASSAL.search;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collection;

import org.apache.commons.io.IOUtils;
import org.jsoup.Jsoup;

/**
 * Parses image tags out of an HTML file or string. Used to add the image filenames to a list, e.g. of images used.
 */
public class HTMLImageFinder {
  String fileName = "";
  String fileString;

  /**
   * @param file - Prepares to parse an HTML file in the module
   */
  public HTMLImageFinder(File file) {
    fileName = file.getName();
    final DataArchive mda = GameModule.getGameModule().getDataArchive();
    try (InputStream inner = mda.getInputStream(file.getPath());
         InputStream in = new BufferedInputStream(inner)) {
      fileString = IOUtils.toString(in, StandardCharsets.UTF_8);
    }
    catch (IOException e) {
      ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Error.not_found", "HTMLImageFinder"), file.getName(), e)); //NON-NLS
      fileString = "<html>...</html>"; //NON-NLS
    }
  }

  /**
   * @param string Prepares to parse an html string
   */
  public HTMLImageFinder(String string) {
    if (string.toLowerCase().contains("<html>")) { //NON-NLS
      fileString = string;
    }
    else {
      fileString = "<html>" + string.trim() + "</html>"; //NON-NLS
    }
  }

  /**
   * Parses the HTML and adds any image filenames to the collection
   * @param s Collection to add image filenames to.
   */
  public void addImageNames(Collection<String> s) {
    s.addAll(Jsoup.parse(fileString).getElementsByTag("img").eachAttr("src")); //NON-NLS
  }
}
