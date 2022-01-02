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
import VASSAL.search.SearchTarget;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.swing.SwingUtils;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.StringUtils;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import java.awt.Frame;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ListKeyCommandsDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  public ListKeyCommandsDialog(Frame owner, List<Object[]> rows) {
    super(owner, Resources.getString("Editor.ListKeyCommands.remove_unused_images"), true);

    final JTable table = new JTable(
      rows.toArray(new Object[0][]),
      new Object[] { "Key Command", "Source Name", "Source Description" }
    );
    table.setAutoCreateRowSorter(true);

    final JPanel panel = new JPanel(new MigLayout("fill", "[]", "[]unrel[]"));  //NON-NLS

    final JScrollPane scroll = new JScrollPane(table);
    panel.add(scroll, "grow, push, wrap");

    final JButton ok = new JButton(Resources.getString("General.ok"));
    ok.addActionListener(e -> dispose());

    final JButton help = new JButton(Resources.getString("General.help"));

    final JPanel buttonPanel = new JPanel(new MigLayout("ins 0", "push[]rel[]push", "")); // NON-NLS
    buttonPanel.add(ok, "tag ok, sg 1"); //$NON-NLS-1$//
    buttonPanel.add(help, "tag help, sg 1");
    panel.add(buttonPanel, "growx"); // NON-NLS

    setLayout(new MigLayout("insets dialog, fill")); // NON-NLS
    add(panel, "grow"); // NON-NLS

    SwingUtils.repack(this);
  }

  private static void checkSearchTarget(SearchTarget target, List<Object[]> list) {
    final List<NamedKeyStroke> keys = target.getNamedKeyStrokeList();
    if (keys != null) {
      for (final NamedKeyStroke k : keys) {
        if (k != null) {
          final String s = k.isNamed() ? k.getName() : KeyNamer.getKeyString(k.getStroke());
          if (!StringUtils.isEmpty(s)) { // Could check a filter here?
            String name = null;
            String desc = null;

            if (target instanceof AbstractConfigurable) {
              name = ((AbstractConfigurable)target).getConfigureName();
            }
            else if (target instanceof GamePiece) {
              name = ((GamePiece)target).getName();
              if (target instanceof Decorator) {
                desc = ((Decorator) target).getDescription();
              }
            }

            list.add(new Object[] { s, name, desc });
          }
        }
      }
    }
  }

  private static void checkForKeyCommands(AbstractConfigurable target, List<Object[]> list) {
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

  private static void recursivelyFindKeyCommands(AbstractBuildable target, List<Object[]> list) {
    for (final Buildable b : target.getBuildables()) {
      if (b instanceof AbstractConfigurable) {
        checkForKeyCommands((AbstractConfigurable)b, list);
      }

      if (b instanceof AbstractBuildable) {
        recursivelyFindKeyCommands((AbstractBuildable)b, list);
      }
    }
  }

  public static List<Object[]> findAllKeyCommands() {
    final List<Object[]> keyCommandList = new ArrayList<>();
    recursivelyFindKeyCommands(GameModule.getGameModule(), keyCommandList);
    return keyCommandList;
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
}
