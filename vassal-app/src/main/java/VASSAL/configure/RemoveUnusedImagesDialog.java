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
      new JLabel("The following image files appear to be unreferenced from the module. They may however be" +
        "referenced in custom code, or be the masks for Non-Rectangular traits.");

    final JLabel keepy   = new JLabel("Files to keep");
    final JLabel excrete = new JLabel("Files to remove");

    final GameModule module = GameModule.getGameModule();
    SortedSet<String> unused = new TreeSet<>();
    SortedSet<String> buggerOff = new TreeSet<>();
    Collections.addAll(unused, module.getDataArchive().getImageNames());
    unused.removeAll(module.getAllImageNames());

    DefaultListModel lm = new DefaultListModel();
    for (String iname : unused) {
      lm.addElement(iname);
    }

    DefaultListModel lm2 = new DefaultListModel();

    final JList keepyMcKeepface = new JList(lm);
    final JScrollPane scroll = new JScrollPane(keepyMcKeepface);

    final JList excretable = new JList(lm2);
    final JScrollPane scroll2 = new JScrollPane(excretable);

    final JButton bugger = new JButton ("-->");
    bugger.addActionListener(e -> {
      int[] indices = keepyMcKeepface.getSelectedIndices();
      int lastSelect = -1;
      for (int idx : indices) {
        String thing = (String)lm.getElementAt(idx);
        unused.remove(thing);
        buggerOff.add(thing);
        if (lastSelect < 0) {
          lastSelect = idx;
        }
      }
      lm.removeAllElements();
      lm2.removeAllElements();
      for (String iname : unused) {
        lm.addElement(iname);
      }
      for (String iname : buggerOff) {
        lm2.addElement(iname);
      }
      if (lastSelect >= 0) {
        if (lastSelect < keepyMcKeepface.getModel().getSize()) {
          keepyMcKeepface.setSelectedIndex(lastSelect);
        }
        else if (lastSelect > 0) {
          keepyMcKeepface.setSelectedIndex(lastSelect - 1);
        }
      }
    });

    final JButton unBugger = new JButton ("<--");
    unBugger.addActionListener(e -> {
      int[] indices = excretable.getSelectedIndices();
      int lastSelect = -1;
      for (int idx : indices) {
        String thing = (String)lm2.getElementAt(idx);
        buggerOff.remove(thing);
        unused.add(thing);
        if (lastSelect < 0) {
          lastSelect = idx;
        }
      }
      lm.removeAllElements();
      lm2.removeAllElements();
      for (String iname : unused) {
        lm.addElement(iname);
      }
      for (String iname : buggerOff) {
        lm2.addElement(iname);
      }
      if (lastSelect >= 0) {
        if (lastSelect < excretable.getModel().getSize()) {
          excretable.setSelectedIndex(lastSelect);
        }
        else if (lastSelect > 0) {
          excretable.setSelectedIndex(lastSelect - 1);
        }
      }
    });

    final JButton ok = new JButton("Remove Files");
    ok.addActionListener(e -> {
      final ArchiveWriter aw = module.getDataArchive().getWriter();

      final File dir =
        new File(new File(aw.getName()).getParent(), "removed");
      dir.mkdir();

      for (String u : buggerOff) {
        GameModule.getGameModule().warn("- Removing: " + u);
        System.out.println("Removing: " + u);

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

      if (!buggerOff.isEmpty()) {
        GameModule.getGameModule().setDirty(true);
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
        .addComponent(keepy)
        .addComponent(excrete)
        .addGroup(layout.createSequentialGroup()
          .addGap(0, 0, Integer.MAX_VALUE)
          .addComponent(scroll)
          .addGap(0, 0, Integer.MAX_VALUE)
          .addGroup(layout.createSequentialGroup()
            .addComponent(bugger)
            .addComponent(unBugger))
          .addGap(0, 0, Integer.MAX_VALUE)
          .addComponent(scroll2)
          .addGap(0, 0, Integer.MAX_VALUE))
        .addGroup(layout.createSequentialGroup()
          .addGap(0, 0, Integer.MAX_VALUE)
          .addComponent(ok)
          .addComponent(cancel)
          .addGap(0, 0, Integer.MAX_VALUE)));
        
    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .addComponent(text)
        .addComponent(keepy)
        .addComponent(excrete)
        .addComponent(scroll)
        .addGroup(layout.createSequentialGroup()
          .addComponent(bugger)
          .addComponent(unBugger))
        .addComponent(scroll2)
        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
        .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE, false)
          .addComponent(ok)
          .addComponent(cancel)));

    layout.linkSize(new Component[]{ ok, cancel });

    add(panel);

    pack();
  }
}
