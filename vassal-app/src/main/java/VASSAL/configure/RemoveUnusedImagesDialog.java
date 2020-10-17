package VASSAL.configure;

import java.awt.Frame;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.SortedSet;
import java.util.TreeSet;
import java.nio.file.Files;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import net.miginfocom.swing.MigLayout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.DataArchive;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.icon.IconFamily;
import VASSAL.tools.swing.FlowLabel;

public class RemoveUnusedImagesDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private static final Logger logger = LoggerFactory.getLogger(RemoveUnusedImagesDialog.class);

  public RemoveUnusedImagesDialog(Frame owner) {
    super(owner, Resources.getString("Editor.UnusedImages.remove_unused_images"), true);
  
    final FlowLabel text =
      new FlowLabel(Resources.getString("Editor.UnusedImages.unused_1"));

    final GameModule module = GameModule.getGameModule();
    SortedSet<String> unused = new TreeSet<>();
    SortedSet<String> buggerOff = new TreeSet<>();
    Collections.addAll(unused, module.getDataArchive().getImageNames());
    unused.removeAll(module.getAllImageNames());

    DefaultListModel<String> lm = new DefaultListModel<>();
    lm.addAll(unused);

    DefaultListModel<String> lm2 = new DefaultListModel<>();

    final JList<String> keepyMcKeepface = new JList<>(lm);
    final JScrollPane scroll = new JScrollPane(keepyMcKeepface);
    scroll.setBorder(BorderFactory.createTitledBorder(Resources.getString("Editor.UnusedImages.files_to_keep")));

    final JList<String> excretable = new JList<>(lm2);
    final JScrollPane scroll2 = new JScrollPane(excretable);
    scroll2.setBorder(BorderFactory.createTitledBorder(Resources.getString("Editor.UnusedImages.files_to_remove")));

    final JButton bugger = new JButton(IconFactory.getIcon("go-next", IconFamily.XSMALL));
    final JButton unBugger = new JButton(IconFactory.getIcon("go-previous", IconFamily.XSMALL));
    final JButton ok = new JButton(Resources.getString("Editor.UnusedImages.remove_files"));
    final JButton cancel = new JButton(Resources.getString("General.cancel"));

    ok.addActionListener(e -> {
      final ArchiveWriter aw = module.getDataArchive().getWriter();

      final File dir =
        new File(new File(aw.getName()).getParent(), "removed");
      dir.mkdir();

      for (String u : buggerOff) {
        GameModule.getGameModule().warn("- " + Resources.getString("Editor.UnusedImages.removing", u));
        System.out.println(Resources.getString("Editor.UnusedImages.removing", u));

        try (InputStream in = aw.getInputStream(DataArchive.IMAGE_DIR + u)) {
          Files.copy(in, dir.toPath().resolve(u));
        }
        catch (IOException ex) {
          logger.error("Augh!", ex); //NON-NLS, obviously
        }

        aw.removeImage(u);
      }

      if (!buggerOff.isEmpty()) {
        GameModule.getGameModule().setDirty(true);
      }

      RemoveUnusedImagesDialog.this.dispose();
    });

    unBugger.addActionListener(e -> {
      int[] indices = excretable.getSelectedIndices();
      int lastSelect = -1;
      for (int idx : indices) {
        String thing = lm2.getElementAt(idx);
        buggerOff.remove(thing);
        unused.add(thing);
        if (lastSelect < 0) {
          lastSelect = idx;
        }
      }

      lm.removeAllElements();
      lm2.removeAllElements();

      lm.addAll(unused);
      lm2.addAll(buggerOff);

      if (lastSelect >= 0) {
        if (lastSelect < excretable.getModel().getSize()) {
          excretable.setSelectedIndex(lastSelect);
        }
        else if (lastSelect > 0) {
          excretable.setSelectedIndex(lastSelect - 1);
        }
      }

      ok.setEnabled(!lm2.isEmpty());
    });

    bugger.addActionListener(e -> {
      int[] indices = keepyMcKeepface.getSelectedIndices();
      int lastSelect = -1;
      for (int idx : indices) {
        String thing = lm.getElementAt(idx);
        unused.remove(thing);
        buggerOff.add(thing);
        if (lastSelect < 0) {
          lastSelect = idx;
        }
      }

      lm.removeAllElements();
      lm2.removeAllElements();

      lm.addAll(unused);
      lm2.addAll(buggerOff);

      if (lastSelect >= 0) {
        if (lastSelect < keepyMcKeepface.getModel().getSize()) {
          keepyMcKeepface.setSelectedIndex(lastSelect);
        }
        else if (lastSelect > 0) {
          keepyMcKeepface.setSelectedIndex(lastSelect - 1);
        }
      }

      ok.setEnabled(!lm2.isEmpty());
    });

    keepyMcKeepface.addListSelectionListener(e -> {
      bugger.setEnabled(!keepyMcKeepface.isSelectionEmpty());
    });

    bugger.setEnabled(!keepyMcKeepface.isSelectionEmpty());

    excretable.addListSelectionListener(e -> {
      unBugger.setEnabled(!excretable.isSelectionEmpty());
    });

    unBugger.setEnabled(!excretable.isSelectionEmpty());
    ok.setEnabled(!lm2.isEmpty());

    cancel.addActionListener(e -> RemoveUnusedImagesDialog.this.dispose());

    final JPanel panel = new JPanel(new MigLayout("insets dialog", "[]rel[]rel[]", "[]unrel[]unrel[]"));

    panel.add(text, "span, wrap");

    panel.add(scroll, "grow, push, sizegroup list");
    panel.add(bugger, "align center, flowy, split 2");
    panel.add(unBugger, "align center");
    panel.add(scroll2, "grow, push, sizegroup list, wrap");

    panel.add(ok, "tag ok, span, split");
    panel.add(cancel, "tag cancel");

    add(panel);
    pack();
  }
}
