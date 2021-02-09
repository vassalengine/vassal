package VASSAL.configure;

import java.awt.Frame;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.util.Arrays;
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

import VASSAL.tools.image.ImageUtils;
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

  private final DefaultListModel<String> keepModel = new DefaultListModel<>();
  private final DefaultListModel<String> dumpModel = new DefaultListModel<>();

  private final SortedSet<String> keep = new TreeSet<>();
  private final SortedSet<String> dump = new TreeSet<>();

  private final JButton ok;

  private final GameModule module;

  public RemoveUnusedImagesDialog(Frame owner) {
    super(owner, Resources.getString("Editor.UnusedImages.remove_unused_images"), true);

    final FlowLabel text =
      new FlowLabel(Resources.getString("Editor.UnusedImages.unused_1"));

    module = GameModule.getGameModule();
    for (final String filename : module.getDataArchive().getImageNames()) {
      if (ImageUtils.hasImageSuffix(filename)) {
        keep.add(filename);
      }
      else {
        keep.add(filename + ImageUtils.GIF_SUFFIX);
      }
    }

    keep.removeAll(module.getAllImageNames());

    keepModel.addAll(keep);

    final JList<String> keepList = new JList<>(keepModel);
    final JScrollPane keepScroll = new JScrollPane(keepList);
    keepScroll.setBorder(BorderFactory.createTitledBorder(Resources.getString("Editor.UnusedImages.files_to_keep")));

    final JList<String> dropList = new JList<>(dumpModel);
    final JScrollPane dropScroll = new JScrollPane(dropList);
    dropScroll.setBorder(BorderFactory.createTitledBorder(Resources.getString("Editor.UnusedImages.files_to_remove")));

    final JButton dropButton = new JButton(IconFactory.getIcon("go-next", IconFamily.XSMALL)); //NON-NLS
    final JButton keepButton = new JButton(IconFactory.getIcon("go-previous", IconFamily.XSMALL)); //NON-NLS

    ok = new JButton(Resources.getString("Editor.UnusedImages.remove_files"));
    final JButton cancel = new JButton(Resources.getString("General.cancel"));

    ok.addActionListener(e -> removeImages());

    keepButton.addActionListener(e -> updateSelection(dropList, dumpModel, dump, keep));
    dropButton.addActionListener(e -> updateSelection(keepList, keepModel, keep, dump));

    keepList.addListSelectionListener(e -> dropButton.setEnabled(!keepList.isSelectionEmpty()));

    dropButton.setEnabled(!keepList.isSelectionEmpty());

    dropList.addListSelectionListener(e -> keepButton.setEnabled(!dropList.isSelectionEmpty()));

    keepButton.setEnabled(!dropList.isSelectionEmpty());
    ok.setEnabled(!dumpModel.isEmpty());

    cancel.addActionListener(e -> dispose());

    final JPanel panel = new JPanel(new MigLayout("insets dialog", "[]rel[]rel[]", "[]unrel[]unrel[]"));  //NON-NLS

    panel.add(text, "span, wrap"); //NON-NLS

    panel.add(keepScroll, "grow, push, sizegroup list"); //NON-NLS
    panel.add(dropButton, "align center, flowy, split 2"); //NON-NLS
    panel.add(keepButton, "align center"); //NON-NLS
    panel.add(dropScroll, "grow, push, sizegroup list, wrap"); //NON-NLS

    panel.add(ok, "tag ok, span, split"); //NON-NLS
    panel.add(cancel, "tag cancel"); //NON-NLS

    add(panel);
    pack();
  }

  private void updateSelection(JList srclist, DefaultListModel<String> srcmodel, SortedSet<String> src, SortedSet<String> dst) {
    final int[] indices = srclist.getSelectedIndices();
    final int lastSelect = indices[indices.length - 1];

    Arrays.stream(indices).forEach(i -> {
      final String item = srcmodel.getElementAt(i);
      src.remove(item);
      dst.add(item);
    });

    keepModel.removeAllElements();
    dumpModel.removeAllElements();

    keepModel.addAll(keep);
    dumpModel.addAll(dump);

    srclist.setSelectedIndex(Math.max(0, Math.min(lastSelect, srcmodel.getSize()) - 1));

    ok.setEnabled(!dumpModel.isEmpty());
  }

  private void removeImages() {
    final ArchiveWriter aw = module.getDataArchive().getWriter();

    final File dir = new File(new File(aw.getName()).getParent(), "removed");
    dir.mkdir();

    for (final String u : dump) {
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

    if (!dump.isEmpty()) {
      GameModule.getGameModule().setDirty(true);
    }

    dispose();
  }
}
