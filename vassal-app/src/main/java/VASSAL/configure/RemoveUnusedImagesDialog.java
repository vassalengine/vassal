package VASSAL.configure;

import java.awt.Component;
import java.awt.Frame;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.DefaultListModel;
import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.LayoutStyle;

import VASSAL.tools.DataArchive;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.io.IOUtils;

public class RemoveUnusedImagesDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private static final Logger logger = LoggerFactory.getLogger(RemoveUnusedImagesDialog.class);

  public RemoveUnusedImagesDialog(Frame owner) {
    super(owner, "Remove Unused Images", true);
  
    final JLabel text =
      new JLabel("The following images are unused. Remove them?");

    final GameModule module = GameModule.getGameModule();
    final SortedSet<String> unused = new TreeSet<>();
    Collections.addAll(unused, module.getDataArchive().getImageNames());
    unused.removeAll(module.getImageNames());

    final DefaultListModel lm = new DefaultListModel();
    for (String iname : unused) {
      lm.addElement(iname);
    }
    
    final JList list = new JList(lm);
    final JScrollPane scroll = new JScrollPane(list);
 
    final JButton ok = new JButton("Ok");
    ok.addActionListener(e -> {
      final ArchiveWriter aw = module.getDataArchive().getWriter();

      final File dir =
        new File(new File(aw.getName()).getParent(), "removed");
      dir.mkdir();

      for (String u : unused) {
        System.out.println("removing: " + u);

        InputStream in = null;
        FileOutputStream out = null;
        try {
          in = aw.getInputStream(DataArchive.IMAGE_DIR + u);
          out = new FileOutputStream(new File(dir, u));
          IOUtils.copy(in, out);
        }
        catch (IOException ex) {
          logger.error("Augh!", ex);
        }
        finally {
          IOUtils.closeQuietly(in);
          IOUtils.closeQuietly(out);
        }

        aw.removeImage(u);
      }

      RemoveUnusedImagesDialog.this.dispose();
    });

    final JButton cancel = new JButton("Cancel");
    cancel.addActionListener(e -> RemoveUnusedImagesDialog.this.dispose());

    final JPanel panel = new JPanel();
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutoCreateGaps(true);
    layout.setAutoCreateContainerGaps(true);

    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.Alignment.LEADING, true)
        .addComponent(text)
        .addComponent(scroll)
        .addGroup(layout.createSequentialGroup()
          .addGap(0, 0, Integer.MAX_VALUE)
          .addComponent(ok)
          .addComponent(cancel)
          .addGap(0, 0, Integer.MAX_VALUE)));
        
    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .addComponent(text)
        .addComponent(scroll)
        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
        .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE, false)
          .addComponent(ok)
          .addComponent(cancel)));

    layout.linkSize(new Component[]{ ok, cancel });

    add(panel);

    pack();
  }
}
