package VASSAL.configure;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.KeyNamer;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.PieceSlot;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.launch.ExtensionEditorWindow;
import VASSAL.search.SearchTarget;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.icon.IconFamily;
import VASSAL.tools.io.FileUtils;
import VASSAL.tools.swing.FlowLabel;
import VASSAL.tools.swing.SwingUtils;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import java.awt.Frame;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

public class ListKeyCommandsDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private static final Logger logger = LoggerFactory.getLogger(ListKeyCommandsDialog.class);

  private final DefaultListModel<String> keepModel = new DefaultListModel<>();
  private final DefaultListModel<String> dumpModel = new DefaultListModel<>();

  private final SortedSet<String> keep = new TreeSet<>();
  private final SortedSet<String> dump = new TreeSet<>();

  private final JButton ok;

  private final DataArchive archive;

  private final JButton dropAllButton;
  private final JButton keepAllButton;

  private final Map<String, Entry> displayIndex = new HashMap<>();
  private final JLabel keepTotalLabel = new JLabel();
  private final JLabel dropTotalLabel = new JLabel();

  public ListKeyCommandsDialog(Frame owner) {
    super(owner, Resources.getString("Editor.ListKeyCommands.remove_unused_images"), true);

    final FlowLabel text =
      new FlowLabel(Resources.getString("Editor.ListKeyCommands.unused_1"));

    AbstractBuildable parent;
    if (owner instanceof ExtensionEditorWindow) {
      parent = ((ExtensionEditorWindow) owner).getExtension();
      archive = ((ExtensionEditorWindow) owner).getExtension().getDataArchive().getWriter();
    }
    else {
      parent = GameModule.getGameModule();
      archive = GameModule.getGameModule().getArchiveWriter();
    }

    Collections.addAll(keep, findAllKeyCommands());

    /*
    for (final String filename : parent.getAllImageNames()) {
      keep.remove(filename);
      if (!ImageUtils.hasImageSuffix(filename)) {
        keep.remove(filename + ImageUtils.GIF_SUFFIX);
      }
    }

    // keep now contains a list of file names. Convert this to a list of display names
    // And build an index of Display names back to real names
    final SortedSet<String> keep2 = new TreeSet<>(keep);
    keep.clear();
    for (final String file : keep2) {
      final Entry entry = new Entry(file, archive);
      keep.add(entry.getDisplayName());
      displayIndex.put(entry.getDisplayName(), entry);
    }
    */

    keepModel.addAll(keep);

    final JList<String> keepList = new JList<>(keepModel);
    final JScrollPane keepScroll = new JScrollPane(keepList);
    keepScroll.setBorder(BorderFactory.createTitledBorder(Resources.getString("Editor.ListKeyCommands.files_to_keep")));

    final JList<String> dropList = new JList<>(dumpModel);
    final JScrollPane dropScroll = new JScrollPane(dropList);
    dropScroll.setBorder(BorderFactory.createTitledBorder(Resources.getString("Editor.ListKeyCommands.files_to_remove")));

    final JButton dropButton = new JButton(IconFactory.getIcon("go-next", IconFamily.SMALL)); //NON-NLS
    final JButton keepButton = new JButton(IconFactory.getIcon("go-previous", IconFamily.SMALL)); //NON-NLS

    dropAllButton = new JButton(IconFactory.getIcon("go-last", IconFamily.SMALL)); //NON-NLS
    keepAllButton = new JButton(IconFactory.getIcon("go-first", IconFamily.SMALL)); //NON-NLS

    dropAllButton.setEnabled(!keepModel.isEmpty());
    keepAllButton.setEnabled(!dumpModel.isEmpty());

    dropAllButton.addActionListener(e -> {
      if (keepList.getModel().getSize() > 0) {
        keepList.setSelectionInterval(0, keepList.getModel().getSize() - 1);
      }
      updateSelection(keepList, keepModel, keep, dump);
    });

    keepAllButton.addActionListener(e -> {
      if (dropList.getModel().getSize() > 0) {
        dropList.setSelectionInterval(0, dropList.getModel().getSize() - 1);
      }
      updateSelection(dropList, dumpModel, dump, keep);
    });

    ok = new JButton(Resources.getString("Editor.ListKeyCommands.remove_files"));
    final JButton cancel = new JButton(Resources.getString("General.cancel"));
    final JButton help = new JButton(Resources.getString("General.help"));

    ok.addActionListener(e -> removeImages());

    keepButton.addActionListener(e -> updateSelection(dropList, dumpModel, dump, keep));
    dropButton.addActionListener(e -> updateSelection(keepList, keepModel, keep, dump));

    keepList.addListSelectionListener(e -> dropButton.setEnabled(!keepList.isSelectionEmpty()));
    keepList.setVisibleRowCount(keepList.getModel().getSize());

    dropButton.setEnabled(!keepList.isSelectionEmpty());

    dropList.addListSelectionListener(e -> keepButton.setEnabled(!dropList.isSelectionEmpty()));

    keepButton.setEnabled(!dropList.isSelectionEmpty());
    ok.setEnabled(!dumpModel.isEmpty());

    cancel.addActionListener(e -> dispose());
    help.addActionListener(e -> help());

    final JPanel panel = new JPanel(new MigLayout("ins 4, fill", "[]rel[]rel[]", "[]unrel[]unrel[]"));  //NON-NLS
    panel.setBorder(BorderFactory.createEtchedBorder());

    panel.add(text, "span, wrap"); //NON-NLS

    panel.add(keepScroll, "grow, push, sizegroup list"); //NON-NLS
    panel.add(dropAllButton, "align center, flowy, split 4"); // NON-NLS
    panel.add(dropButton, "align center"); //NON-NLS
    panel.add(keepButton, "align center"); //NON-NLS
    panel.add(keepAllButton, "align center"); //NON-NLS
    panel.add(dropScroll, "grow, push, sizegroup list, wrap"); //NON-NLS

    panel.add(keepTotalLabel, "center"); // NON-NLS
    panel.add(dropTotalLabel, "skip 1,center,wrap"); // NON-NLS


    final JPanel buttonPanel = new JPanel(new MigLayout("fill", "push[]rel[]rel[]push")); // NON-NLS
    buttonPanel.add(ok, "tag ok,sg 1"); //$NON-NLS-1$//
    buttonPanel.add(cancel, "tag cancel,sg 1"); //$NON-NLS-1$//
    buttonPanel.add(help, "tag help,sg 1");
    panel.add(buttonPanel, "span 3,grow"); // NON-NLS

    setLayout(new MigLayout("fill")); // NON-NLS
    add(panel, "grow"); // NON-NLS

    updateButtons();

    SwingUtils.repack(this);
  }


  private void checkSearchTarget(SearchTarget target, List<String> list) {
    final List<NamedKeyStroke> keys = target.getNamedKeyStrokeList();
    if (keys != null) {
      for (final NamedKeyStroke k : keys) {
        if (k != null) {
          final String s = k.isNamed() ? k.getName() : KeyNamer.getKeyString(k.getStroke());
          if (!StringUtils.isEmpty(s)) { // Could check a filter here?

            String display;

            display = "<html><b>" + s + "</b>";
            display += ": ";

            if (target instanceof AbstractConfigurable) {
              display += ((AbstractConfigurable)target).getConfigureName();
            }
            else if (target instanceof GamePiece) {
              display += ((GamePiece)target).getName();
              if (target instanceof Decorator) {
                display += ((Decorator) target).getDescription();
              }
            }
            display += "</html>";

            list.add(display);
          }
        }
      }
    }
  }

  private void checkForKeyCommands(AbstractConfigurable target, List<String> list) {
    GamePiece p;
    boolean protoskip;
    if (target instanceof GamePiece) {
      p = (GamePiece) target;
      protoskip = false;
    }
    else if (target instanceof PieceSlot) {
      p = ((PieceSlot)target).getPiece();
      protoskip = false;
    }
    else if (target instanceof PrototypeDefinition) {
      p = ((PrototypeDefinition)target).getPiece();
      protoskip = true;
    }
    else  {
      checkSearchTarget(target, list);
      return;
    }

    // We're going to search Decorator from inner-to-outer (BasicPiece-on-out), so that user sees the traits hit in
    // the same order they're listed in the PieceDefiner window. So we first traverse them in the "normal" direction
    // outer to inner and make a list in the order we want to traverse it (for architectural reasons, just traversing
    // with getOuter() would take us inside of prototypes inside a piece, which we don't want).
    final List<GamePiece> pieces = new ArrayList<>();
    pieces.add(p);
    while (p instanceof Decorator) {
      p = ((Decorator) p).getInner();
      pieces.add(p);
    }
    Collections.reverse(pieces);

    for (final GamePiece piece : pieces) {
      if (!protoskip) { // Skip the fake "Basic Piece" on a Prototype definition
        if (piece instanceof SearchTarget) {
          checkSearchTarget((SearchTarget)piece, list);
        }
      }
      protoskip = false;
    }
  }

  private void recursivelyFindKeyCommands(AbstractBuildable target, List<String> list) {
    for (final Buildable b : target.getBuildables()) {
      if (b instanceof AbstractConfigurable) {
        checkForKeyCommands((AbstractConfigurable)b, list);
      }

      if (b instanceof AbstractBuildable) {
        recursivelyFindKeyCommands((AbstractBuildable)b, list);
      }
    }
  }


  private String[] findAllKeyCommands() {
    final List<String> keyCommandList = new ArrayList<>();

    recursivelyFindKeyCommands(GameModule.getGameModule(), keyCommandList);

    return keyCommandList.toArray(new String [0]);
  }

  private void help() {
    HelpFile hf = null;
    try {
      hf = new HelpFile(null, new File(
        new File(Documentation.getDocumentationBaseDir(), "ReferenceManual"),
        "ListKeyCommands.html"));
    }
    catch (MalformedURLException ex) {
      ErrorDialog.bug(ex);
    }

    (new ShowHelpAction(hf.getContents(), null)).actionPerformed(null);

  }

  private void updateSelection(JList<String> srclist, DefaultListModel<String> srcmodel, SortedSet<String> src, SortedSet<String> dst) {
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

    updateButtons();
  }

  private void updateButtons() {
    ok.setEnabled(!dumpModel.isEmpty());
    dropAllButton.setEnabled(!keepModel.isEmpty());
    keepAllButton.setEnabled(!dumpModel.isEmpty());

    long keepTotal = 0;
    long dropTotal = 0;

    //for (final String keepFile : keep) {
    //  keepTotal += displayIndex.get(keepFile).getSize();
    //}
    //for (final String keepFile : dump) {
    //  dropTotal += displayIndex.get(keepFile).getSize();
    //}

    keepTotalLabel.setText("(" + FileUtils.byteCountToDisplaySize(keepTotal) + ")");
    dropTotalLabel.setText("(" + FileUtils.byteCountToDisplaySize(dropTotal) + ")");
  }

  private void removeImages() {
    /*
    final File dir = new File(new File(archive.getName()).getParent(), "removed");
    dir.mkdir();

    for (final String uName : dump) {
      final String u = displayIndex.get(uName).getFileName();

      GameModule.getGameModule().warn("- " + Resources.getString("Editor.ListKeyCommands.removing", uName));

      try (InputStream in = archive.getWriter().getInputStream(DataArchive.IMAGE_DIR + u)) {
        Files.copy(in, dir.toPath().resolve(u));
      }
      catch (IOException ex) {
        logger.error("Augh!", ex); //NON-NLS, obviously
      }

      archive.getWriter().removeImage(u);
    }

    if (!dump.isEmpty()) {
      GameModule.getGameModule().setDirty(true);
    }

    dispose();
     */
  }

  private static class Entry {
    private final String fileName;
    private final String displayName;
    private long size;

    public Entry(String fileName, DataArchive archive) {
      this.fileName = fileName;
      try {
        size = archive.getArchive().getCompressedSize(DataArchive.IMAGE_DIR + fileName);
      }
      catch (IOException ignored) {
        size = -1;
      }
      displayName = fileName + " (" + FileUtils.byteCountToDisplaySize(size) + ")";
    }

    public String getFileName() {
      return fileName;
    }

    public String getDisplayName() {
      return displayName;
    }

    public long getSize() {
      return size;
    }

    @Override
    public String toString() {
      return getDisplayName();
    }
  }
}
