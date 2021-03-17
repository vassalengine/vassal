/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.build.widget;

import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Collection;

import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.ComponentView;
import javax.swing.text.Element;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLEditorKit;

import org.apache.commons.io.IOUtils;

import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.OpIcon;
import VASSAL.tools.imageop.SourceOp;
import VASSAL.tools.swing.DataArchiveHTMLEditorKit;

/**
 * An HtmlChart is used for displaying html information for the module. The
 * charts are loaded as html files stored in the DataArchive. As a subclass of
 * Widget, a Chart may be added to any Widget, but it may not contain children
 * of its own
 */
public class HtmlChart extends Widget implements MouseListener {
  public static final String NAME = "chartName"; //NON-NLS
  public static final String FILE = "fileName"; //NON-NLS

  private String fileName;
  private JScrollPane scroller;
  private JEditorPane htmlWin;

  private boolean isURL() {
    return htmlWin.getDocument().getProperty("stream") != null; //NON-NLS
  }

  private void setText(String text) {
    htmlWin.setText(text);
    // ensure hyperlink engine knows we are no longer at the last URL
    htmlWin.getDocument().putProperty("stream", null); //NON-NLS
    htmlWin.revalidate();
  }

  private void setFile(String fname) {
    setText(getFile(fname));
  }

  private String getFile(final String fname) {
    if (fname == null) return null;
    final String localizedFileName = GameModule.getGameModule().getResourcePathFinder().findHelpFileName(fname);
    final DataArchive mda = GameModule.getGameModule().getDataArchive();
    String s = null;
    try (InputStream inner = mda.getInputStream(localizedFileName);
         InputStream in = new BufferedInputStream(inner)) {
      s = IOUtils.toString(in, StandardCharsets.UTF_8);
    }
    catch (final IOException e) {
      ErrorDialog.dataWarning(new BadDataReport(this, Resources.getString("Error.not_found", "Chart"), fname, e)); //NON-NLS
    }

    return s;
  }

  // Warning: Creating a JEditorPane with a "jar" url or using setPage()
  // with a jar URL will leave a resource open in the MOD file, making it
  // impossible to save or rename it. This might be acceptable for people
  // playing a module, but is unacceptable for editors; they can only save
  // their work to a new file. Therefore, we read the entire file instead
  // of simply using:
  //    GameModule.getGameModule().getDataArchive().getURL(fileName);
  @Override
  public Component getComponent() {
    if (htmlWin == null) {
      htmlWin = new JEditorPane();
      htmlWin.setEditable(false);
      htmlWin.setContentType("text/html");
      htmlWin.setEditorKit(new DataArchiveHTMLEditorKit(GameModule.getGameModule().getDataArchive()));

      htmlWin.addHyperlinkListener(new HtmlChartHyperlinkListener());
      htmlWin.addMouseListener(this);

      setFile(fileName);

      scroller = new ScrollPane(htmlWin);
      scroller.getViewport().setPreferredSize(htmlWin.getPreferredSize());
      scroller.getViewport().setAlignmentY(0.0F);

      final Font f = new JLabel().getFont();
      final FontMetrics fm = htmlWin.getFontMetrics(f);
      scroller.getVerticalScrollBar().setUnitIncrement(fm.getHeight() * 3); //BR// Mousewheel scrolls 3 lines of default JLabel font height
    }
    return scroller;
  }

  public String getFileName() {
    return fileName;
  }

  @Override
  public void addTo(Buildable parent) {
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.HTMLChart.component_type");
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ChartWindow.html", "HtmlChart"); //NON-NLS
  }

  @Override
  public void setAttribute(String key, Object val) {
    if (NAME.equals(key)) {
      setConfigureName((String) val);
    }
    else if (FILE.equals(key)) {
      if (val instanceof File) {
        val = ((File) val).getName();
      }
      fileName = (String) val;
      if (htmlWin != null) {
        setFile(fileName);
      }
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  /**
   * The Attributes of a Chart are:
   * <dl>
   *  <dt><code>NAME</code></dt><dd>for the name of the chart</dd>
   *  <dt><code>FILE</code></dt><dd>for the name of the HTML file
   *  in the {@link VASSAL.tools.DataArchive}</dd>
   * </dl>
   */
  @Override
  public String[] getAttributeNames() {
    return new String[]{NAME, FILE};
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{Resources.getString("Editor.name_label"), Resources.getString("Editor.HTMLChart.html_file")};
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{String.class, File.class};
  }

  @Override
  public String getAttributeValueString(String name) {
    if (NAME.equals(name)) {
      return getConfigureName();
    }
    else if (FILE.equals(name)) {
      return fileName;
    }
    return null;
  }

  protected void maybePopup(MouseEvent event) {
    if (event.isPopupTrigger()) {
      final JPopupMenu popup = new JPopupMenu();
      final JMenuItem item = new JMenuItem(Resources.getString("Editor.HTMLChart.return_to_default_page"));

      // Return to default page
      item.addActionListener(e -> setFile(fileName));

      popup.add(item);
      if (event.getComponent().isShowing()) {
        popup.show(event.getComponent(), event.getX(), event.getY());
      }
    }
  }

  @Override
  public void mousePressed(MouseEvent e) {
    maybePopup(e);
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    maybePopup(e);
  }

  @Override
  public void mouseClicked(MouseEvent e) {
  }

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }

  public class HtmlChartHyperlinkListener implements HyperlinkListener {
    @Override
    public void hyperlinkUpdate(HyperlinkEvent event) {
      if (event.getEventType() != HyperlinkEvent.EventType.ACTIVATED) {
        return;
      }

      final String desc = event.getDescription();
      if ((!isURL() && desc.indexOf('/') < 0) || event.getURL() == null) {
        final int hash = desc.lastIndexOf('#');
        if (hash < 0) {
          // no anchor
          setFile(desc);
        }
        else if (hash > 0) {
          // browse to the part before the anchor
          setFile(desc.substring(0, hash));
        }

        if (hash != -1) {
          // we have an anchor
          htmlWin.scrollToReference(desc.substring(hash + 1));
        }
      }
      else {
        try {
          htmlWin.setPage(event.getURL());
        }
        catch (final IOException ex) {
          ReadErrorDialog.error(ex, event.getURL().toString());
        }
        htmlWin.revalidate();
      }
    }
  }

  /**
   * Find all of our image references and register them
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    final HTMLImageFinder h = new HTMLImageFinder(new File(fileName));
    h.addImageNames(s);
  }

  /**
   * Extended HTML Editor kit to extend the <src> tag to display images
   * from the module DataArchive where no pathname included in the image name.
   * The image is placed on a label and returned as a ComponentView. An
   * ImageView cannot be used as the standard Java HTML Renderer can only
   * display Images from an external URL.
   *
   * @deprecated Use {@link VASSAL.tools.swing.DataArchiveHTMLEditorKit} instead.
   */
  @Deprecated(since = "2020-10-10", forRemoval = true)
  public static class XTMLEditorKit extends HTMLEditorKit {
    private static final long serialVersionUID = 1L;

    @Override
    public ViewFactory getViewFactory() {
      return new XTMLFactory();
    }

    public static class XTMLFactory extends HTMLFactory implements ViewFactory {
      @Override
      public View create(Element element) {
        final HTML.Tag kind = (HTML.Tag) (element.getAttributes().getAttribute(javax.swing.text.StyleConstants.NameAttribute));

        if (kind != null && element.getName().equals("img")) {  //NON-NLS
          final String imageName = (String) element.getAttributes().getAttribute(HTML.Attribute.SRC);
          if (!imageName.contains("/")) {
            return new ImageComponentView(element);
          }
        }
        return super.create(element);
      }

      public static class ImageComponentView extends ComponentView {
        protected String imageName;
        protected SourceOp srcOp;

        /**
         * Very basic Attribute handling only. Expand as needed.
         */
        public ImageComponentView(Element e) {
          super(e);
          imageName = (String) e.getAttributes()
                                .getAttribute(HTML.Attribute.SRC);
          srcOp = (imageName == null || imageName.isBlank()) ? null : Op.load(imageName);
        }

        @Override
        protected Component createComponent() {
          final JLabel label = new JLabel();
          label.setIcon(new OpIcon(srcOp));
          return label;
        }
      }
    }
  }
}
