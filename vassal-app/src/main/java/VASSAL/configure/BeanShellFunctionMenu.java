package VASSAL.configure;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.BoardPicker;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.counters.Attachment;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Labeler;
import VASSAL.counters.PieceDefiner;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.FunctionBuilder;
import VASSAL.script.expression.IntBuilder;
import VASSAL.script.expression.StrBuilder;
import VASSAL.tools.menu.MenuScroller;
import VASSAL.tools.swing.SwingUtils;

import javax.swing.JDialog;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import java.awt.Dimension;
import java.util.Arrays;
import java.util.List;

import static VASSAL.configure.BeanShellExpressionConfigurer.Option;

public class BeanShellFunctionMenu extends JPopupMenu {
  private static final long serialVersionUID = 1L;

  public static final String COMMENT = "Comment";

  protected static final String[] NO_HINTS = new String[0];

  protected static final String[] GENERAL_PME_HINTS = {
    Resources.getString("Editor.BeanShell.warning2"),
    Resources.getString("Editor.BeanShell.warning3")
  };

  protected static final String[] SUM_COUNT_HINTS = {
    Resources.getString("Editor.BeanShell.warning"),
    Resources.getString("Editor.BeanShell.warning2"),
    Resources.getString("Editor.BeanShell.warning3")
  };

  protected static final String[] SUM_COUNT_MAP_HINTS = {
    Resources.getString("Editor.BeanShell.warning"),
    Resources.getString("Editor.BeanShell.map_warning"),
    Resources.getString("Editor.BeanShell.warning2"),
    Resources.getString("Editor.BeanShell.warning3")
  };

  protected static final String[] AUDIT_OPTION_HINTS = {
    Resources.getString("Editor.BeanShell.debug.audit.hint1"),
    Resources.getString("Editor.BeanShell.debug.audit.hint2"),
    Resources.getString("Editor.BeanShell.debug.audit.hint3"),
    Resources.getString("Editor.BeanShell.debug.audit.hint4"),
    Resources.getString("Editor.BeanShell.debug.audit.hint5")
  };

  protected static final String[] ATTACHMENT_INDEX_HINTS = {
    Resources.getString("Editor.BeanShell.attach_index.hint1"),
  };

  protected static final String[] NAME_HINTS = {
    Resources.getString("Editor.BeanShell.debug.name_hint1"),
  };

  protected static final String[] NAME_PME_HINTS = {
    Resources.getString("Editor.BeanShell.debug.name_hint1"),
    Resources.getString("Editor.BeanShell.warning2"),
    Resources.getString("Editor.BeanShell.warning3")
  };
  protected static int maxScrollItems = 0;
  protected BeanShellExpressionConfigurer configurer;
  protected EditablePiece target;

  enum PropertyType { PIECE, GLOBAL, VASSAL, ALL };

  public BeanShellFunctionMenu(EditablePiece target, BeanShellExpressionConfigurer configurer) {
    super();

    this.target = target;
    this.configurer = configurer;

    final JMenu constantMenu = new JMenu(Resources.getString("Editor.BeanShell.constant"));
    final JMenuItem integerItem = new JMenuItem(Resources.getString("Editor.BeanShell.number"));
    integerItem.setToolTipText(Resources.getString("Editor.BeanShell.a_number"));
    integerItem.addActionListener(e -> buildInteger());
    constantMenu.add(integerItem);

    final JMenuItem stringItem = new JMenuItem(Resources.getString("Editor.BeanShell.string"));
    stringItem.setToolTipText(Resources.getString("Editor.BeanShell.a_string"));
    stringItem.addActionListener(e -> buildString());
    constantMenu.add(stringItem);
    addSeparator(constantMenu);

    final JMenuItem trueItem = new JMenuItem(Resources.getString("Editor.BeanShell.true"));
    trueItem.addActionListener(e -> buildBoolean(true));
    constantMenu.add(trueItem);

    final JMenuItem falseItem = new JMenuItem(Resources.getString("Editor.BeanShell.false"));
    falseItem.addActionListener(e -> buildBoolean(false));
    constantMenu.add(falseItem);

    addSeparator(constantMenu);
    addFunction(constantMenu, COMMENT, Resources.getString("Editor.BeanShell.inline_comment"), new String[] { Resources.getString("Editor.BeanShell.comment")}, "", NO_HINTS, new Option[] {Option.COMMENT}); //NON-NLS

    add(constantMenu);

    final JMenu propertyMenu = new JMenu(Resources.getString("Editor.BeanShell.property"));

    if (isPieceContext() || requiresGenericPiecePropertyMenu()) {
      final JMenu pieceMenu = new JMenu(Resources.getString("Editor.BeanShell.piece_property"));
      addPieceProps(pieceMenu, target);
      if (requiresGenericPiecePropertyMenu()) {
        addGenericPiecePropMenu(pieceMenu);
      }
      propertyMenu.add(pieceMenu);

    }

    final JMenu globalsMenu = new JMenu(Resources.getString("Editor.BeanShell.global_property"));
    buildGlobalMenu(globalsMenu, GameModule.getGameModule(), true);
    MenuScroller.setScrollerFor(globalsMenu, getMaxScrollItems(), 100);

    propertyMenu.add(globalsMenu);

    final JMenu vassalMenu = new JMenu(Resources.getString("Editor.BeanShell.vassal_property"));
    for (final String prop : GameModule.getGameModule().getPropertyNames()) {
      addProp(vassalMenu, prop, PropertyType.VASSAL);
    }
    propertyMenu.add(vassalMenu);
    add(propertyMenu);

    final JMenu operatorMenu = new JMenu(Resources.getString("Editor.BeanShell.operator"));
    addOperator(operatorMenu, "+", Resources.getString("Editor.BeanShell.add"));
    addOperator(operatorMenu, "-", Resources.getString("Editor.BeanShell.subtract"));
    addOperator(operatorMenu, "*", Resources.getString("Editor.BeanShell.multiply"));
    addOperator(operatorMenu, "/", Resources.getString("Editor.BeanShell.divide"));
    addOperator(operatorMenu, "%", Resources.getString("Editor.BeanShell.modulus"));
    add(operatorMenu);

    final JMenu comparisonMenu = new JMenu(Resources.getString("Editor.BeanShell.comparison"));
    addOperator(comparisonMenu, "==", Resources.getString("Editor.BeanShell.equals"));
    addOperator(comparisonMenu, "!=", Resources.getString("Editor.BeanShell.not_equals"));
    addOperator(comparisonMenu, ">",  Resources.getString("Editor.BeanShell.greater_than"));
    addOperator(comparisonMenu, ">=", Resources.getString("Editor.BeanShell.greater_than_or_equal_to"));
    addOperator(comparisonMenu, "<",  Resources.getString("Editor.BeanShell.less_than"));
    addOperator(comparisonMenu, "<=", Resources.getString("Editor.BeanShell.less_than_or_equal_to"));
    addOperator(comparisonMenu, "=~", Resources.getString("Editor.BeanShell.matches_regular_expression"));
    addOperator(comparisonMenu, "!~", Resources.getString("Editor.BeanShell.does_not_match_regular_expression"));
    add(comparisonMenu);

    final JMenu logicalMenu = new JMenu(Resources.getString("Editor.BeanShell.logical"));
    addOperator(logicalMenu, "&&", Resources.getString("Editor.BeanShell.and"));
    addOperator(logicalMenu, "||", Resources.getString("Editor.BeanShell.or"));
    addOperator(logicalMenu, "!", Resources.getString("Editor.BeanShell.not"));
    addOperator(logicalMenu, "(", Resources.getString("Editor.BeanShell.left_parenthesis"));
    addOperator(logicalMenu, ")", Resources.getString("Editor.BeanShell.right_parenthesis"));
    add(logicalMenu);

    final JMenu mathMenu = new JMenu(Resources.getString("Editor.BeanShell.math"));
    addFunction(mathMenu, "Math.abs", Resources.getString("Editor.BeanShell.abs"), new String[] { Resources.getString("Editor.BeanShell.number")}, "(n)"); //NON-NLS
    addFunction(mathMenu, "Math.min", Resources.getString("Editor.BeanShell.min"), new String[] { Resources.getString("Editor.BeanShell.number1"), Resources.getString("Editor.BeanShell.number2") }, "(m, n)"); //NON-NLS
    addFunction(mathMenu, "Math.max", Resources.getString("Editor.BeanShell.max"), new String[] { Resources.getString("Editor.BeanShell.number1"), Resources.getString("Editor.BeanShell.number2") }, "(m, n)"); //NON-NLS

    final JMenu propMenu = new JMenu(Resources.getString("Editor.BeanShell.property"));
    addFunction(propMenu, "GetProperty", Resources.getString("Editor.BeanShell.getproperty"), new String[] { Resources.getString("Editor.BeanShell.property_name") }, "(prop)", NAME_HINTS); //NON-NLS
    addFunction(propMenu, "GetString", Resources.getString("Editor.BeanShell.getstring"), new String[] { Resources.getString("Editor.BeanShell.property_name") }, "(prop)", NAME_HINTS); //NON-NLS
    addSeparator(propMenu);
    addFunction(propMenu, "GetAttachProperty", Resources.getString("Editor.BeanShell.getattachmentpropertyf"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_name")}, "(attachment, prop)", NAME_HINTS); //NON-NLS
    addFunction(propMenu, "GetAttachProperty", Resources.getString("Editor.BeanShell.getattachmentproperty"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.index_name") }, "(attachment, prop, index)", NAME_HINTS); //NON-NLS
    addFunction(propMenu, "GetAttachProperty", Resources.getString("Editor.BeanShell.getattachmentpropertyb"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.basic_name") }, "(attachment, prop, basicName)", NAME_HINTS); //NON-NLS
    addFunction(propMenu, "GetAttachProperty", Resources.getString("Editor.BeanShell.getattachmentpropertye"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(attachment, prop, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.NONE, Option.PME}); //NON-NLS
    addSeparator(propMenu);
    addFunction(propMenu, "GetMapProperty", Resources.getString("Editor.BeanShell.getmapproperty"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.map_name") }, "(prop, map)", NAME_HINTS); //NON-NLS
    addSeparator(propMenu);
    addFunction(propMenu, "GetZoneProperty", Resources.getString("Editor.BeanShell.getzoneproperty"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.zone_name") }, "(prop, zone)", NAME_HINTS); //NON-NLS
    addFunction(propMenu, "GetZoneProperty", Resources.getString("Editor.BeanShell.getzonemapproperty"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.zone_name"), Resources.getString("Editor.BeanShell.map_name") }, "(prop, zone, map)", NAME_HINTS); //NON-NLS

    final JMenu stringMenu = new JMenu(Resources.getString("Editor.BeanShell.string"));
    addFunction(stringMenu, ".length", Resources.getString("Editor.BeanShell.Slength"), new String[] {Resources.getString("Editor.BeanShell.target_string")}, "()"); //NON-NLS
    addFunction(stringMenu, ".contains", Resources.getString("Editor.BeanShell.Scontains"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.search_string") }, "(string)"); //NON-NLS
    addFunction(stringMenu, ".isEmpty", Resources.getString("Editor.BeanShell.Sempty"), new String[] { Resources.getString("Editor.BeanShell.target_string") }, "()"); //NON-NLS
    addFunction(stringMenu, ".isBlank", Resources.getString("Editor.BeanShell.Sblank"), new String[] { Resources.getString("Editor.BeanShell.target_string") }, "()"); //NON-NLS
    addFunction(stringMenu, ".startsWith", Resources.getString("Editor.BeanShell.Sstartswith"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.search_string") }, "(string)"); //NON-NLS
    addFunction(stringMenu, ".endsWith", Resources.getString("Editor.BeanShell.Sendswith"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.search_string") }, "(string)"); //NON-NLS
    addFunction(stringMenu, ".matches", Resources.getString("Editor.BeanShell.Smatches"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.regex") }, "(regExpr)"); //NON-NLS
    addFunction(stringMenu, ".indexOf", Resources.getString("Editor.BeanShell.Sindexof"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.search_string") }, "(string)"); //NON-NLS
    addFunction(stringMenu, ".lastIndexOf", Resources.getString("Editor.BeanShell.Slastindexof"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.search_string") }, "(string)"); //NON-NLS
    addFunction(stringMenu, ".substring", Resources.getString("Editor.BeanShell.Ssubstring"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.starting_position") }, "(start)"); //NON-NLS
    addFunction(stringMenu, ".substring", Resources.getString("Editor.BeanShell.Ssubstring2"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.starting_position"), Resources.getString("Editor.BeanShell.ending_position") }, "(start, end)"); //NON-NLS
    addFunction(stringMenu, ".replace", Resources.getString("Editor.BeanShell.Sreplace"), new String[] { Resources.getString("Editor.BeanShell.target_string"), Resources.getString("Editor.BeanShell.to_find"), Resources.getString("Editor.BeanShell.to_replace") }, "(old, new)"); //NON-NLS
    addFunction(stringMenu, ".trim", Resources.getString("Editor.BeanShell.Strim"), new String[] {Resources.getString("Editor.BeanShell.target_string")}, "()"); //NON-NLS
    addFunction(stringMenu, ".toUpperCase", Resources.getString("Editor.BeanShell.StoUpperCase"), new String[] {Resources.getString("Editor.BeanShell.target_string")}, "()"); //NON-NLS
    addFunction(stringMenu, ".toLowerCase", Resources.getString("Editor.BeanShell.StoLowerCase"), new String[] {Resources.getString("Editor.BeanShell.target_string")}, "()"); //NON-NLS
    addFunction(stringMenu, ".toString", Resources.getString("Editor.BeanShell.Stostring"), new String[] { Resources.getString("Editor.BeanShell.target_string") }, "()"); //NON-NLS

    final JMenu randomMenu = new JMenu(Resources.getString("Editor.BeanShell.random"));
    addFunction(randomMenu, "Random", Resources.getString("Editor.BeanShell.random1"), new String[] { Resources.getString("Editor.BeanShell.randomhi") }, "(x)"); //NON-NLS
    addFunction(randomMenu, "Random", Resources.getString("Editor.BeanShell.random2"), new String[] { Resources.getString("Editor.BeanShell.randomlo"), Resources.getString("Editor.BeanShell.randomhi") }, "(x, y)"); //NON-NLS
    addSeparator(randomMenu);
    addFunction(randomMenu, "IsRandom", Resources.getString("Editor.BeanShell.random3"), new String[] {}, "()"); //NON-NLS
    addFunction(randomMenu, "IsRandom", Resources.getString("Editor.BeanShell.random4"), new String[] { Resources.getString("Editor.BeanShell.randomp") }, "(p)"); //NON-NLS


    final JMenu attachmentMenu = new JMenu(Resources.getString("Editor.BeanShell.by_attachment"));

    addFunction(attachmentMenu, "SumAttach", Resources.getString("Editor.BeanShell.sum10"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_name") }, "(name, prop)", NAME_HINTS); //NON-NLS
    addFunction(attachmentMenu, "SumAttach", Resources.getString("Editor.BeanShell.sum10e"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(name, prop, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.NONE, Option.PME}); //NON-NLS
    addSeparator(attachmentMenu);
    addFunction(attachmentMenu, "CountAttach", Resources.getString("Editor.BeanShell.sum11a"), new String[] { Resources.getString("Editor.BeanShell.attachment_name") }, "(name)", NAME_HINTS); //NON-NLS
    addFunction(attachmentMenu, "CountAttach", Resources.getString("Editor.BeanShell.sum11"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_name")}, "(name, prop)", NAME_HINTS); //NON-NLS
    addFunction(attachmentMenu, "CountAttach", Resources.getString("Editor.BeanShell.sum12"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(name, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.PME}); //NON-NLS
    addFunction(attachmentMenu, "CountAttach", Resources.getString("Editor.BeanShell.sum12e"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(name, prop, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.NONE, Option.PME}); //NON-NLS
    addSeparator(attachmentMenu);
    addFunction(attachmentMenu, "MinAttach", Resources.getString("Editor.BeanShell.sum14"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_name") }, "(name, prop)", NAME_HINTS); //NON-NLS
    addFunction(attachmentMenu, "MaxAttach", Resources.getString("Editor.BeanShell.sum13"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_name") }, "(name, prop)", NAME_HINTS); //NON-NLS

    final JMenu locationMenu = new JMenu(Resources.getString("Editor.BeanShell.by_location"));
    if (isPieceContext()) {
      addFunction(locationMenu, "SumLocation", Resources.getString("Editor.BeanShell.sum15"), new String[] { Resources.getString("Editor.BeanShell.property_name")}, "(prop)", NAME_HINTS);
      addFunction(locationMenu, "SumLocation", Resources.getString("Editor.BeanShell.sum16"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.PME}); //NON-NLS
      addSeparator(locationMenu);
    }
    addFunction(locationMenu, "SumLocation", Resources.getString("Editor.BeanShell.sum17"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.location_name"), Resources.getString("Editor.BeanShell.map_name") }, "(prop, loc, map)", NAME_HINTS);
    addFunction(locationMenu, "SumLocation", Resources.getString("Editor.BeanShell.sum18"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.location_name"), Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(prop, loc, map, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.NONE, Option.NONE, Option.PME}); //NON-NLS
    addSeparator(locationMenu);
    if (isPieceContext()) {
      addFunction(locationMenu, "CountLocation", Resources.getString("Editor.BeanShell.sum19"), new String[]{}, "()"); //NON-NLS
      addFunction(locationMenu, "CountLocation", Resources.getString("Editor.BeanShell.sum20"), new String[]{Resources.getString("Editor.BeanShell.property_name")}, "(prop)", NAME_HINTS); //NON-NLS
      addFunction(locationMenu, "CountLocation", Resources.getString("Editor.BeanShell.sum21"), new String[]{Resources.getString("Editor.BeanShell.property_match_expression")}, "(expr)", NAME_PME_HINTS, new Option[]{Option.PME}); //NON-NLS
      addFunction(locationMenu, "CountLocation", Resources.getString("Editor.BeanShell.sum22"), new String[]{Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.PME}); //NON-NLS
      addSeparator(locationMenu);
    }
    addFunction(locationMenu, "CountLocation", Resources.getString("Editor.BeanShell.sum23"), new String[] { Resources.getString("Editor.BeanShell.location_name"), Resources.getString("Editor.BeanShell.map_name") }, "(loc, map)", NAME_HINTS);
    addFunction(locationMenu, "CountLocation", Resources.getString("Editor.BeanShell.sum24"), new String[] { Resources.getString("Editor.BeanShell.location_name"), Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_name") }, "(loc, map, prop)", NAME_HINTS);
    addFunction(locationMenu, "CountLocation", Resources.getString("Editor.BeanShell.sum25"), new String[] { Resources.getString("Editor.BeanShell.location_name"), Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(loc, map, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.NONE, Option.PME}); //NON-NLS
    addFunction(locationMenu, "CountLocation", Resources.getString("Editor.BeanShell.sum26"), new String[] { Resources.getString("Editor.BeanShell.location_name"), Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(loc, map, prop, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.NONE, Option.NONE, Option.PME}); //NON-NLS

    final JMenu stackMenu = new JMenu(Resources.getString("Editor.BeanShell.by_stack"));
    addFunction(stackMenu, "SumStack", Resources.getString("Editor.BeanShell.sum1"), new String[] { Resources.getString("Editor.BeanShell.property_name") }, "(prop)", NAME_HINTS); //NON-NLS
    addFunction(stackMenu, "SumStack", Resources.getString("Editor.BeanShell.sum1e"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.PME}); //NON-NLS
    addSeparator(stackMenu);
    addFunction(stackMenu, "CountStack", Resources.getString("Editor.BeanShell.sum6"), new String[] { }, "()"); //NON-NLS
    addFunction(stackMenu, "CountStack", Resources.getString("Editor.BeanShell.sum7"), new String[] { Resources.getString("Editor.BeanShell.property_name") }, "(prop)", NAME_HINTS); //NON-NLS
    addFunction(stackMenu, "CountStack", Resources.getString("Editor.BeanShell.sum6e"), new String[] { Resources.getString("Editor.BeanShell.property_match_expression") }, "(expr)", NAME_PME_HINTS, new Option[] {Option.PME}); //NON-NLS
    addFunction(stackMenu, "CountStack", Resources.getString("Editor.BeanShell.sum7e"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(prop, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.PME}); //NON-NLS

    final JMenu matMenu = new JMenu(Resources.getString("Editor.BeanShell.by_mat"));
    addFunction(matMenu, "SumMat", Resources.getString("Editor.BeanShell.sum8"), new String[] { Resources.getString("Editor.BeanShell.property_name") }, "(prop)", NAME_HINTS); //NON-NLS
    addFunction(matMenu, "SumMat", Resources.getString("Editor.BeanShell.sum8e"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(prop, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.PME}); //NON-NLS
    addSeparator(matMenu);
    addFunction(matMenu, "CountMat", Resources.getString("Editor.BeanShell.sum9b"), new String[] { }, "()"); //NON-NLS
    addFunction(matMenu, "CountMat", Resources.getString("Editor.BeanShell.sum9"), new String[] { Resources.getString("Editor.BeanShell.property_name") }, "(prop)", NAME_HINTS); //NON-NLS
    addFunction(matMenu, "CountMat", Resources.getString("Editor.BeanShell.sum9e"), new String[] { Resources.getString("Editor.BeanShell.property_match_expression") }, "(expr)", NAME_PME_HINTS, new Option[] {Option.PME}); //NON-NLS
    addFunction(matMenu, "CountMat", Resources.getString("Editor.BeanShell.sum9ee"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(prop, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.PME}); //NON-NLS

    final JMenu zoneMenu = new JMenu(Resources.getString("Editor.BeanShell.by_zone"));
    if (isPieceContext()) {
      addFunction(zoneMenu, "SumZone", Resources.getString("Editor.BeanShell.sum27"), new String[] { Resources.getString("Editor.BeanShell.property_name")}, "(prop)", NAME_HINTS);
      addFunction(zoneMenu, "SumZone", Resources.getString("Editor.BeanShell.sum28"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.PME}); //NON-NLS
      addSeparator(zoneMenu);
    }
    addFunction(zoneMenu, "SumZone", Resources.getString("Editor.BeanShell.sum29"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.zone_name"), Resources.getString("Editor.BeanShell.map_name") }, "(prop, zone, map)", NAME_HINTS);
    addFunction(zoneMenu, "SumZone", Resources.getString("Editor.BeanShell.sum30"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.zone_name"), Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(prop, zone, map, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.NONE, Option.NONE, Option.PME}); //NON-NLS
    addSeparator(zoneMenu);

    if (isPieceContext()) {
      addFunction(zoneMenu, "CountZone", Resources.getString("Editor.BeanShell.sum31"), new String[] { }, "()"); //NON-NLS
      addFunction(zoneMenu, "CountZone", Resources.getString("Editor.BeanShell.sum32"), new String[] { Resources.getString("Editor.BeanShell.property_name")}, "(prop)", NAME_HINTS); //NON-NLS
      addFunction(zoneMenu, "CountZone", Resources.getString("Editor.BeanShell.sum33"), new String[] { Resources.getString("Editor.BeanShell.property_match_expression")}, "(expr)", NAME_PME_HINTS, new Option[]{Option.PME}); //NON-NLS
      addFunction(zoneMenu, "CountZone", Resources.getString("Editor.BeanShell.sum34"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.PME}); //NON-NLS
      addSeparator(zoneMenu);
    }
    addFunction(zoneMenu, "CountZone", Resources.getString("Editor.BeanShell.sum35"), new String[] { Resources.getString("Editor.BeanShell.zone_name"), Resources.getString("Editor.BeanShell.map_name") }, "(zone, map)", NAME_HINTS); //NON-NLS
    addFunction(zoneMenu, "CountZone", Resources.getString("Editor.BeanShell.sum36"), new String[] { Resources.getString("Editor.BeanShell.zone_name"), Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_name")}, "(zone, map, prop)", NAME_HINTS); //NON-NLS
    addFunction(zoneMenu, "CountZone", Resources.getString("Editor.BeanShell.sum37"), new String[] { Resources.getString("Editor.BeanShell.zone_name"), Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(zone, map, expr)", NAME_PME_HINTS, new Option[] { Option.NONE, Option.NONE, Option.PME }); //NON-NLS
    addFunction(zoneMenu, "CountZone", Resources.getString("Editor.BeanShell.sum38"), new String[] { Resources.getString("Editor.BeanShell.zone_name"), Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(zone, map, prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.NONE, Option.NONE, Option.PME}); //NON-NLS

    final JMenu mapMenu = new JMenu(Resources.getString("Editor.BeanShell.by_map"));
    if (isPieceContext()) {
      addFunction(mapMenu, "SumMap", Resources.getString("Editor.BeanShell.sum39"), new String[] { Resources.getString("Editor.BeanShell.property_name")}, "(prop)", NAME_HINTS);
      addFunction(mapMenu, "SumMap", Resources.getString("Editor.BeanShell.sum40"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.PME}); //NON-NLS
      addSeparator(mapMenu);
    }
    addFunction(mapMenu, "SumMap", Resources.getString("Editor.BeanShell.sum41"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.map_name") }, "(prop, map)", NAME_HINTS);
    addFunction(mapMenu, "SumMap", Resources.getString("Editor.BeanShell.sum42"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(prop, map, expr)", NAME_PME_HINTS, new Option[] {Option.NONE, Option.NONE, Option.PME}); //NON-NLS
    addSeparator(mapMenu);

    if (isPieceContext()) {
      addFunction(mapMenu, "CountMap", Resources.getString("Editor.BeanShell.sum43"), new String[] { }, "()"); //NON-NLS
      addFunction(mapMenu, "CountMap", Resources.getString("Editor.BeanShell.sum44"), new String[] { Resources.getString("Editor.BeanShell.property_name")}, "(prop)", NAME_HINTS); //NON-NLS
      addFunction(mapMenu, "CountMap", Resources.getString("Editor.BeanShell.sum45"), new String[] { Resources.getString("Editor.BeanShell.property_match_expression")}, "(expr)", NAME_PME_HINTS, new Option[]{Option.PME}); //NON-NLS
      addFunction(mapMenu, "CountMap", Resources.getString("Editor.BeanShell.sum46"), new String[] { Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.PME}); //NON-NLS
      addSeparator(mapMenu);
    }
    addFunction(mapMenu, "CountMap", Resources.getString("Editor.BeanShell.sum47"), new String[] { Resources.getString("Editor.BeanShell.map_name") }, "(map)", NAME_HINTS); //NON-NLS
    addFunction(mapMenu, "CountMap", Resources.getString("Editor.BeanShell.sum48"), new String[] { Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_name")}, "(map, prop)", NAME_HINTS); //NON-NLS
    addFunction(mapMenu, "CountMap", Resources.getString("Editor.BeanShell.sum49"), new String[] { Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(map, expr)", NAME_PME_HINTS, new Option[] { Option.NONE, Option.PME }); //NON-NLS
    addFunction(mapMenu, "CountMap", Resources.getString("Editor.BeanShell.sum50"), new String[] { Resources.getString("Editor.BeanShell.map_name"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(map, prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.NONE, Option.PME}); //NON-NLS


    final JMenu rangedMenu = new JMenu(Resources.getString("Editor.BeanShell.ranged"));
    if (isPieceContext()) {
      addFunction(rangedMenu, "SumRange", Resources.getString("Editor.BeanShell.ranged1"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range"), Resources.getString("Editor.BeanShell.property_name") }, "(min, max, prop)", NAME_HINTS);
      addFunction(rangedMenu, "SumRange", Resources.getString("Editor.BeanShell.ranged2"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(min, max, prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.NONE, Option.NONE, Option.PME});
      addSeparator(rangedMenu);
      addFunction(rangedMenu, "SumRangePx", Resources.getString("Editor.BeanShell.ranged1p"), new String[]{Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range"), Resources.getString("Editor.BeanShell.property_name") }, "(min, max, prop)", NAME_HINTS);
      addFunction(rangedMenu, "SumRangePx", Resources.getString("Editor.BeanShell.ranged2p"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(min, max, prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.NONE, Option.NONE, Option.PME});
      addSeparator(rangedMenu);
      addFunction(rangedMenu, "CountRange", Resources.getString("Editor.BeanShell.ranged3"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range")}, "(min, max)");
      addFunction(rangedMenu, "CountRange", Resources.getString("Editor.BeanShell.ranged4"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range"), Resources.getString("Editor.BeanShell.property_name")}, "(min, max, prop)", NAME_HINTS);
      addFunction(rangedMenu, "CountRange", Resources.getString("Editor.BeanShell.ranged5"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(min, max, expr)", GENERAL_PME_HINTS, new Option[]{Option.NONE, Option.NONE, Option.PME});
      addFunction(rangedMenu, "CountRange", Resources.getString("Editor.BeanShell.ranged6"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(min, max, prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.NONE, Option.NONE, Option.PME});
      addSeparator(rangedMenu);
      addFunction(rangedMenu, "CountRangePx", Resources.getString("Editor.BeanShell.ranged3p"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range")}, "(min, max)");
      addFunction(rangedMenu, "CountRangePx", Resources.getString("Editor.BeanShell.ranged4p"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range"), Resources.getString("Editor.BeanShell.property_name")}, "(min, max, prop)", NAME_HINTS);
      addFunction(rangedMenu, "CountRangePx", Resources.getString("Editor.BeanShell.ranged5p"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(min, max, expr)", GENERAL_PME_HINTS, new Option[]{Option.NONE, Option.NONE, Option.PME});
      addFunction(rangedMenu, "CountRangePx", Resources.getString("Editor.BeanShell.ranged6p"), new String[]{ Resources.getString("Editor.BeanShell.minimum_range"), Resources.getString("Editor.BeanShell.maximum_range"), Resources.getString("Editor.BeanShell.property_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(min, max, prop, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.NONE, Option.NONE, Option.PME});
    }

    final JMenu countMenu = new JMenu(Resources.getString("Editor.BeanShell.sumcount"));
    if (isPieceContext()) {
      countMenu.add(stackMenu);
      countMenu.add(matMenu);
      countMenu.add(attachmentMenu);
    }
    countMenu.add(locationMenu);
    countMenu.add(zoneMenu);
    countMenu.add(mapMenu);
    if (isPieceContext()) {
      countMenu.add(rangedMenu);
    }

    final JMenu rangeMenu = new JMenu(Resources.getString("Editor.BeanShell.range"));
    if (isPieceContext()) {
      addFunction(rangeMenu, "Range", Resources.getString("Editor.BeanShell.range2"), new String[]{Resources.getString("Editor.BeanShell.x"), Resources.getString("Editor.BeanShell.y")}, "(x, y)");
      addFunction(rangeMenu, "Range", Resources.getString("Editor.BeanShell.range8"), new String[] { Resources.getString("Editor.BeanShell.fromx"), Resources.getString("Editor.BeanShell.fromy"), Resources.getString("Editor.BeanShell.tox"), Resources.getString("Editor.BeanShell.toy") }, "(x1, y1, x2, y2)");
      addFunction(rangeMenu, "Range", Resources.getString("Editor.BeanShell.range4"), new String[]{Resources.getString("Editor.BeanShell.attachment_name")}, "(attachment)");
      addFunction(rangeMenu, "Range", Resources.getString("Editor.BeanShell.range4e"), new String[]{Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_match_expression")}, "(attachment, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.PME});

      addSeparator(rangeMenu);
    }
    addFunction(rangeMenu, "Range", Resources.getString("Editor.BeanShell.range6"), new String[] { Resources.getString("Editor.BeanShell.fromx"), Resources.getString("Editor.BeanShell.fromy"), Resources.getString("Editor.BeanShell.tox"), Resources.getString("Editor.BeanShell.toy"), Resources.getString("Editor.BeanShell.map") }, "(x1, y1, x2, y2, map)");
    addSeparator(rangeMenu);

    if (isPieceContext()) {
      addFunction(rangeMenu, "RangePx", Resources.getString("Editor.BeanShell.range1"), new String[] { Resources.getString("Editor.BeanShell.x"), Resources.getString("Editor.BeanShell.y") }, "(x, y)");
      addFunction(rangeMenu, "RangePx", Resources.getString("Editor.BeanShell.range7"), new String[] { Resources.getString("Editor.BeanShell.fromx"), Resources.getString("Editor.BeanShell.fromy"), Resources.getString("Editor.BeanShell.tox"), Resources.getString("Editor.BeanShell.toy") }, "(x1, y1, x2, y2)");
      addFunction(rangeMenu, "RangePx", Resources.getString("Editor.BeanShell.range3"), new String[] { Resources.getString("Editor.BeanShell.attachment_name") }, "(attachment)", NAME_HINTS);
      addFunction(rangeMenu, "RangePx", Resources.getString("Editor.BeanShell.range3e"), new String[] { Resources.getString("Editor.BeanShell.attachment_name"), Resources.getString("Editor.BeanShell.property_match_expression") }, "(attachment, expr)", NAME_PME_HINTS, new Option[]{Option.NONE, Option.PME});
      addSeparator(rangeMenu);
    }
    addFunction(rangeMenu, "RangePx", Resources.getString("Editor.BeanShell.range5"), new String[] { Resources.getString("Editor.BeanShell.fromx"), Resources.getString("Editor.BeanShell.fromy"), Resources.getString("Editor.BeanShell.tox"), Resources.getString("Editor.BeanShell.toy"), Resources.getString("Editor.BeanShell.map") }, "(x1, y1, x2, y2, map)");

    final JMenu functionMenu = new JMenu(Resources.getString("Editor.BeanShell.function"));
    functionMenu.add(mathMenu);
    functionMenu.add(propMenu);
    functionMenu.add(randomMenu);
    functionMenu.add(stringMenu);
    functionMenu.add(countMenu);
    functionMenu.add(rangeMenu);

    addFunction(functionMenu, "?", Resources.getString("Editor.BeanShell.ternary"), new String[] { Resources.getString("Editor.BeanShell.logical_expression"), Resources.getString("Editor.BeanShell.if_true"), Resources.getString("Editor.BeanShell.if_false") }, "(expr ? r1 : r2)"); //NON-NLS
    add(functionMenu);

    final JMenu otherMenu = new JMenu(Resources.getString("Editor.BeanShell.other"));
    addFunction(otherMenu, "Alert", Resources.getString("Editor.BeanShell.debug.alert"),  new String[] { Resources.getString("Editor.BeanShell.text_to_display") }, "(text)"); //NON-NLS
    addFunction(otherMenu, "Alert", Resources.getString("Editor.BeanShell.debug.alert2"), new String[] { Resources.getString("Editor.BeanShell.text_to_display"), Resources.getString("Editor.BeanShell.milliseconds") }, "(text, ms)"); //NON-NLS
    addFunction(otherMenu, "Sleep", Resources.getString("Editor.BeanShell.debug.sleep"),  new String[] { Resources.getString("Editor.BeanShell.milliseconds") }, "(ms)"); //NON-NLS
    addFunction(otherMenu, "Audit", Resources.getString("Editor.BeanShell.debug.audit1"), new String[] { Resources.getString("Editor.BeanShell.text_to_display") }, "(text)"); //NON-NLS
    addFunction(otherMenu, "Audit", Resources.getString("Editor.BeanShell.debug.audit2"), new String[] { Resources.getString("Editor.BeanShell.text_to_display"), Resources.getString("Editor.BeanShell.debug.audit.options") }, "(text, options)", AUDIT_OPTION_HINTS, new Option[]{Option.NONE, Option.NONE});
    addFunction(otherMenu, "Audit", Resources.getString("Editor.BeanShell.debug.audit3"), new String[] { Resources.getString("Editor.BeanShell.text_to_display"), Resources.getString("Editor.BeanShell.debug.audit.match_expression") }, "(text, expr)", NO_HINTS, new Option[]{Option.NONE, Option.PME});
    addFunction(otherMenu, "Audit", Resources.getString("Editor.BeanShell.debug.audit4"), new String[] { Resources.getString("Editor.BeanShell.text_to_display"), Resources.getString("Editor.BeanShell.debug.audit.match_expression"), Resources.getString("Editor.BeanShell.debug.audit.options") }, "(text, expr, options)", AUDIT_OPTION_HINTS, new Option[]{Option.NONE, Option.PME, Option.NONE});
    add(otherMenu);

  }

  protected void addFunction(JMenu menu, final String op, final String desc, final String[] parms, final String parmInfo) {
    addFunction(menu, op, desc, parms, parmInfo, null);
  }

  protected void addFunction(JMenu menu, final String op, final String desc, final String[] parms, final String parmInfo, final String[] hints) {
    final Option[] options = new Option[parms.length];
    Arrays.fill(options, Option.NONE);
    addFunction(menu, op, desc, parms, parmInfo, hints, options);
  }

  protected void addFunction(JMenu menu, final String op, final String desc, final String[] parms, final String parmInfo, final String[] hints, final Option[] options) {
    final JMenuItem item = new JMenuItem(op + ((parmInfo != null && parmInfo.length() == 0) ? "" : " " + parmInfo));
    item.setToolTipText(desc);
    item.addActionListener(e -> buildFunction(op, desc, parms, hints, options));
    menu.add(item);
  }

  protected void addSeparator(JMenu menu) {
    menu.addSeparator();
  }

  protected void buildFunction(String op, String desc, String[] parmDesc, String[] hints, Option[] options) {
    if (parmDesc.length == 0) {
      configurer.insertName(op + "()");
    }
    else {
      final StringConfigurer result = new StringConfigurer(null, "", "");
      new FunctionBuilder(result, (JDialog) configurer.getTopLevelAncestor(), op, desc, parmDesc, target, hints, options, configurer.getSelectedText()).setVisible(true);
      if (result.getValue() != null && result.getValueString().length() > 0) {
        configurer.insertName(result.getValueString());
      }
    }
  }

  protected void buildInteger() {
    final StringConfigurer result = new StringConfigurer(null, "", "");
    new IntBuilder(result, (JDialog) configurer.getTopLevelAncestor()).setVisible(true);
    if (result.getValue() != null && result.getValueString().length() > 0) {
      configurer.insertName(result.getValueString());
    }
  }

  protected void buildString() {
    final StringConfigurer result = new StringConfigurer(null, "", "");
    new StrBuilder(result, (JDialog) configurer.getTopLevelAncestor()).setVisible(true);
    if (result.getValue() != null && result.getValueString().length() > 0) {
      configurer.insertName(result.getValueString());
    }
  }

  protected void buildBoolean(boolean b) {
    configurer.insertName(b ? "true" : "false");
  }

  protected void addOperator(JMenu menu, final String op, String desc) {
    final JMenuItem item = new JMenuItem(op);
    item.setToolTipText(desc);
    item.addActionListener(e -> configurer.insertName(op));
    menu.add(item);
  }


  /**
   * Add straight property name to a menu
   * @param menu parent menu
   * @param propName property name to add
   */
  protected void addProp(JMenu menu, final String propName, PropertyType propertyType) {

    // Ignore any null propNames
    if (propName == null) {
      return;
    }


    // Ignore Duplicates
    for (int i = 0; i < menu.getItemCount(); i++) {
      if (menu.getItem(i).getText().equals(propName)) {
        return;
      }
    }

    if (isPieceContext() && PropertyType.GLOBAL == propertyType) {
      // Adding a Zone-level global property to a piece-level expression
      if (menuZone != null) {
        final JMenu pieceMenu = new JMenu(propName);
        final JMenuItem sameZone = new JMenuItem(Resources.getString("Editor.BeanShell.same_zone"));
        sameZone.addActionListener(e -> configurer.insertPropertyName(propName));
        final JMenuItem sameItem = new JMenuItem(Resources.getString("Editor.BeanShell.same_map"));
        final String actionName = getActionName(propName, true);
        sameItem.addActionListener(e -> configurer.insertName(actionName));
        final JMenuItem diffItem = new JMenuItem(Resources.getString("Editor.BeanShell.different_map"));
        diffItem.addActionListener(e -> configurer.insertName(actionName));
        pieceMenu.add(sameZone);
        pieceMenu.add(sameItem);
        pieceMenu.add(diffItem);
        insertMenuItem(menu, propName, pieceMenu);
        return;
      }
      // Adding a Map-level global property to a piece-level expression
      else if (menuMap != null) {
        final JMenu pieceMenu = new JMenu(propName);
        final JMenuItem sameItem = new JMenuItem(Resources.getString("Editor.BeanShell.same_map"));
        sameItem.addActionListener(e -> configurer.insertPropertyName(propName));
        final JMenuItem diffItem = new JMenuItem(Resources.getString("Editor.BeanShell.different_map"));
        final String actionName = getActionName(propName);
        diffItem.addActionListener(e -> configurer.insertName(actionName));
        pieceMenu.add(sameItem);
        pieceMenu.add(diffItem);
        insertMenuItem(menu, propName, pieceMenu);
        return;
      }
    }

    final JMenuItem item = new JMenuItem(propName);

    final String actionName = getActionName(propName);
    if (actionName.startsWith("\"") && actionName.endsWith("\"")) {
      item.addActionListener(e -> configurer.insertPropertyName(actionName));
    }
    else {
      item.addActionListener(e -> configurer.insertName(actionName));
    }

    insertMenuItem(menu, propName, item);
  }

  protected void insertMenuItem(JMenu menu, String name, JMenuItem item) {
    int pos = -1;
    for (int i = 0; i < menu.getItemCount() && pos < 0; i++) {
      if (name.compareTo(menu.getItem(i).getText()) <= 0) {
        pos = i;
      }
    }
    menu.add(item, pos);
  }

  protected String getActionName(String propName) {
    return getActionName(propName, false);
  }

  protected String getActionName(String propName, boolean zoneCurrentMapOverride) {
    if (menuZone != null) {
      // Adding a Zone property, use GetZoneProperty
      if (menuMap.equals(configurer.getContext()) || zoneCurrentMapOverride) {
        // Zone on the current Map
        return "GetZoneProperty(\"" + propName + "\", \"" + menuZone.getName() + "\", CurrentMap)";
      }
      else {
        // Zone on a different Map
        return "GetZoneProperty(\"" + propName + "\", \"" + menuZone.getName() + "\", \"" + menuMap.getMapName() + "\")";
      }

    }
    else if (menuMap != null) {
      // Currently adding map-level properties
      if (menuMap.equals(configurer.getContext())) {
        // Adding a map-level property to a component in the same map
        return propName;
      }
      else {
        // Adding a map-level property to a component in a different map
        return "GetMapProperty(\"" + propName + "\", \"" + menuMap.getMapName() + "\")";
      }

    }

    return propName;
  }
  /**
   * Added the property names from an Editable Piece into their
   * own menu
   * @param menu parent menu
   * @param piece Piece containing property names
   */
  protected void addPieceProps(JMenu menu, EditablePiece piece) {
    if (piece == null) {
      return;
    }

    JMenu pieceMenu = null;

    if (piece instanceof PropertyNameSource) {
      final List<String> propNames = ((PropertyNameSource) piece).getPropertyNames();
      for (final String propName : propNames) {
        if (pieceMenu == null) {
          pieceMenu = new JMenu();
          // Provide a meaningful menu text for the dummy basic Piece at the bottom of prototypes
          final String menuText = (piece instanceof BasicPiece && piece.getDescription().isEmpty()) ? Resources.getString("Editor.BasicPiece.trait_description") : piece.getDescription();
          pieceMenu.setText(menuText);
        }
        addProp(pieceMenu, propName, PropertyType.PIECE);
      }

      if (pieceMenu != null) {
        menu.add(pieceMenu);
      }

      if (piece instanceof Decorator) {
        addPieceProps(menu, (EditablePiece) ((Decorator) piece).getInner());
      }
    }
  }

  /**
   * Create a menu of Generic Piece properties applicable to any piece
   * Include properties for all possible traits.
   * provide placeholder names for variably named properties
   *
   * @param menu
   */
  protected void addGenericPiecePropMenu(JMenu menu) {
    final JMenu propMenu = new JMenu(Resources.getString("Editor.BeanShell.generic_piece_propertys"));

    final BasicPiece basicPiece = new BasicPiece();
    addPieceProps(propMenu, basicPiece);

    /*
     * Loop through the available traits in current VASSAL.
     * We create a new PieceDefiner here to ensure PieceDefiner static info has been initialised
     */
    for (final GamePiece piece : (new PieceDefiner()).getTraitList()) {
      final List<String> propNames = ((PropertyNameSource) piece).getPropertyNames();
      if (! propNames.isEmpty()) {
        final JMenu pieceMenu = new JMenu(((Decorator) piece).getBaseDescription());
        for (final String propName : propNames) {
          String displayName = propName;

          // Add placeholders for user defined names
          if (propName.isEmpty()) {
            displayName = "<" + Resources.getString("Editor.BeanShell.prop_name") + ">";
          }
          else if (propName.startsWith("_")) {
            displayName = "<" + Resources.getString((piece instanceof Attachment) ? "Editor.BeanShell.attach_name" : "Editor.BeanShell.trait_name") + ">" + propName;
          }
          else if (piece instanceof Labeler) {
            displayName = "<" + Resources.getString("Editor.BeanShell.label_name") + ">";
          }
          addProp(pieceMenu, displayName, PropertyType.PIECE);
        }
        propMenu.add(pieceMenu);
      }
    }
    menu.add(propMenu);
  }
  /**
   * Create a menu of Global Properties recorded in this module, based on
   * the module build structure
   */

  protected Map menuMap;
  protected Zone menuZone;

  protected void buildGlobalMenu(JMenu parentMenu, AbstractBuildable target, boolean useParentMenu) {

    if (target instanceof Map) {
      menuMap = (Map) target;
    }
    else if (target instanceof Zone) {
      menuZone = (Zone) target;
    }

    final List<Buildable> buildables = target.getBuildables();
    String menuName = ConfigureTree.getConfigureName(target.getClass());
    if (target instanceof AbstractConfigurable) {
      final String n = ((AbstractConfigurable) target).getConfigureName();
      if (n != null && n.length() > 0) {
        menuName += " " + n;
      }
    }
    final JMenu myMenu = new JMenu(menuName);

    // Don't include GameModule's property Names, these are added to a separate menu
    if (!(target instanceof GameModule)) {
      final List<String> propNames = target.getPropertyNames();
      for (final String propName : propNames) {
        addProp(useParentMenu ? parentMenu : myMenu, propName, PropertyType.GLOBAL);
      }
    }

    for (final Buildable b : buildables) {
      if (b instanceof AbstractConfigurable) {
        // Remove 'filler' menu levels due to intermediate holding components
        final boolean useParent = (b instanceof GlobalProperties || b instanceof Board || b instanceof ZonedGrid);
        buildGlobalMenu(useParentMenu ? parentMenu : myMenu, (AbstractConfigurable) b, useParent);
      }
      else if (b instanceof BoardPicker) {
        buildGlobalMenu(myMenu, (AbstractBuildable) b, true);
      }
    }

    if (!useParentMenu && myMenu.getItemCount() > 0) {
      MenuScroller.setScrollerFor(myMenu, getMaxScrollItems(), 100);
      int pos = -1;
      for (int i = 0; i < parentMenu.getItemCount() && pos < 0; i++) {
        if (myMenu.getText().compareTo(parentMenu.getItem(i).getText()) <= 0) {
          pos = i;
        }
      }
      parentMenu.add(myMenu, pos);
    }

    if (target instanceof Map) {
      menuMap = null;
    }
    else if (target instanceof Zone) {
      menuZone = null;
    }
  }

  protected int getMaxScrollItems() {
    if (maxScrollItems == 0) {
      final Dimension itemSize = (new JMenuItem("Testing")).getPreferredSize();
      maxScrollItems = (int) (0.98 * SwingUtils.getScreenBounds(configurer.getControls()).height / itemSize.height);
    }
    return maxScrollItems;
  }

  /**
   * Do we include piece-specific functions in the Function Menu? Only if
   * a) An EditablePiece has been supplied as the target
   * OR
   * b) The calling BeanShellExpressionConfigurer is for a Property Match Expression (since PME get executed on Pieces)
   * 
   * @return true if we show piece specific functions
   */
  protected boolean isPieceContext() {
    return configurer.isPieceContext() || configurer.getOption() == Option.PME;
  }

  /**
   * Does this builder need a Generic piece property menu added to the drop-down.
   * Needed when we are building an expression on a Piece that is not currently being edited
   *
   *  - Needed for all PME's
   *  - Needed if the context level is PIECE, but we are not currently editing a piece
   *
   * @return true if a generic piece property Menu is required
   */
  protected boolean requiresGenericPiecePropertyMenu() {
    return configurer.getOption() == Option.PME || (configurer.isPieceContext() && target == null);
  }
}
