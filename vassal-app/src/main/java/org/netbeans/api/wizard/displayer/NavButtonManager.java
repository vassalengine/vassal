/*
 * NavButtonManager.java       created on Dec 9, 2006
 * 
 */

package org.netbeans.api.wizard.displayer;

import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.NoSuchElementException;
import java.util.logging.Logger;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.UIManager;

import org.netbeans.api.wizard.WizardDisplayer;
import org.netbeans.modules.wizard.MergeMap;
import org.netbeans.modules.wizard.NbBridge;
import org.netbeans.spi.wizard.DeferredWizardResult;
import org.netbeans.spi.wizard.Summary;
import org.netbeans.spi.wizard.Wizard;
import org.netbeans.spi.wizard.WizardException;
import org.netbeans.spi.wizard.WizardObserver;
import org.netbeans.spi.wizard.WizardPanel;
import org.netbeans.spi.wizard.WizardPanelNavResult;

/**
 * Manage the button state and interaction with the wizard.
 * <p>
 * <b><i><font color="red">This class is NOT AN API CLASS.  There is no
 * commitment that it will remain backward compatible or even exist in the
 * future.  The API of this library is in the packages <code>org.netbeans.api.wizard</code>
 * and <code>org.netbeans.spi.wizard</code></font></i></b>.
 *
 * @author stanley@stanleyknutson.com
 */
public class NavButtonManager implements ActionListener
{
    static final String NAME_NEXT      = "next";

    static final String NAME_PREV      = "prev";

    static final String NAME_FINISH    = "finish";

    static final String NAME_CANCEL    = "cancel";

    static final String NAME_CLOSE     = "close";
    
    /** Prefix for the name in deferredStatus */
    static final String DEFERRED_FAILED = "FAILED_";


    private static final Logger logger =
        Logger.getLogger(NavButtonManager.class.getName());

    JButton             next           = null;

    JButton             prev           = null;

    JButton             finish         = null;

    JButton             cancel         = null;

    JButton             help           = null;

    JPanel              buttons        = null;

    // container can be JDialog or JFrame
    private Window      window;

    WizardDisplayerImpl parent;

    String              closeString    = NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                                            WizardDisplayer.class, "Close"); // NOI18N

    boolean suppressMessageDialog = false;
    /**
     * Deferred status of not null means we are waiting for a deferred result to
     * be completed as part of the handling for some button Value of the
     * deferredStatus is the NAME_* constant that triggered the deferred
     * operation.
     */
    String              deferredStatus = null;

    private ActionListener closeHandler = new ActionListener() {
        public void actionPerformed(final ActionEvent e) {
	    final Container top = ((JComponent)e.getSource()).getTopLevelAncestor();
	    if (top instanceof Window) {
		final Window win = (Window) top;
		win.setVisible(false);
		win.dispose();
	    }
	}
    };


    NavButtonManager(WizardDisplayerImpl impl)
    {
        parent = impl;
    }

    protected void buildButtons(Action helpAction)
    {

        next = new JButton(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                              WizardDisplayer.class, "Next_>")); // NOI18N
        next.setName(NAME_NEXT);
        next.setMnemonic(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                              WizardDisplayer.class, "Next_mnemonic").charAt(0)); // NOI18N

        prev = new JButton(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                              WizardDisplayer.class, "<_Prev")); // NOI18N
        prev.setName(NAME_PREV);
        prev.setMnemonic(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                              WizardDisplayer.class, "Prev_mnemonic").charAt(0)); // NOI18N

        finish = new JButton(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                                WizardDisplayer.class, "Finish")); // NOI18N
        finish.setName(NAME_FINISH);
        finish.setMnemonic(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                              WizardDisplayer.class, "Finish_mnemonic").charAt(0)); // NOI18N

        cancel = new JButton(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                                WizardDisplayer.class, "Cancel")); // NOI18N
        cancel.setName(NAME_CANCEL);
        cancel.setMnemonic(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                              WizardDisplayer.class, "Cancel_mnemonic").charAt(0)); // NOI18N

        help = new JButton();
        if (helpAction != null)
        {
            help.setAction(helpAction);
            if (helpAction.getValue(Action.NAME) == null)
            {
                help.setText(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                                WizardDisplayer.class, "Help")); // NOI18N
                help.setMnemonic(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                                      WizardDisplayer.class, "Help_mnemonic").charAt(0)); // NOI18N
            }
        }
        else
        {
            help.setText(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                            WizardDisplayer.class, "Help")); // NOI18N
            help.setMnemonic(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                              WizardDisplayer.class, "Help_mnemonic").charAt(0)); // NOI18N
        }

        next.setDefaultCapable(true);
        prev.setDefaultCapable(true);

        help.setVisible(helpAction != null);

        // Use standard default-button-last order on Aqua L&F
        final boolean aqua = "Aqua".equals(UIManager.getLookAndFeel().getID()); // NOI18N

        buttons = new JPanel()
        {
            private static final long serialVersionUID = -1219524604575467507L;

            public void doLayout()
            {
                Insets ins = getInsets();
                JButton b = aqua ? finish : cancel;

                Dimension n = b.getPreferredSize();
                int y = ((getHeight() - (ins.top + ins.bottom)) / 2) - (n.height / 2);
                int gap = 5;
                int x = getWidth() - (12 + ins.right + n.width);

                b.setBounds(x, y, n.width, n.height);

                b = aqua ? next : finish;
                n = b.getPreferredSize();
                x -= n.width + gap;
                b.setBounds(x, y, n.width, n.height);

                b = aqua ? prev : next;
                n = b.getPreferredSize();
                x -= n.width + gap;
                b.setBounds(x, y, n.width, n.height);

                b = aqua ? cancel : prev;
                n = b.getPreferredSize();
                x -= n.width + gap;
                b.setBounds(x, y, n.width, n.height);

                b = help;
                n = b.getPreferredSize();
                x -= n.width + (gap * 2);
                b.setBounds(x, y, n.width, n.height);
            }
        };
        buttons.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, UIManager
            .getColor("textText"))); // NOI18N

        buttons.add(prev);
        buttons.add(next);
        buttons.add(finish);
        buttons.add(cancel);
        buttons.add(help);

        next.addActionListener(this);
        prev.addActionListener(this);
        finish.addActionListener(this);
        cancel.addActionListener(this);
    }

    void connectListener()
    {
        NavWizardObserver l = new NavWizardObserver();
        Wizard wizard = parent.getWizard();
        l.stepsChanged(wizard);
        l.navigabilityChanged(wizard);
        l.selectionChanged(wizard);
        wizard.addWizardObserver(l);
    }

    private static void configureNavigationButtons(final Wizard wizard, final JButton prev,
                                                   final JButton next, final JButton finish)
    {
        final String nextStep = wizard.getNextStep();
        final int fwdNavMode = wizard.getForwardNavigationMode();

        WizardDisplayerImpl.checkLegalNavMode(fwdNavMode);

        final String problem = wizard.getProblem();

        boolean canContinue = (fwdNavMode & Wizard.MODE_CAN_CONTINUE) != 0;
        boolean canFinish = (fwdNavMode & Wizard.MODE_CAN_FINISH) != 0;
        boolean enableFinish = canFinish && problem == null;
        boolean enableNext = nextStep != null && canContinue && problem == null;
        next.setEnabled(enableNext);
        prev.setEnabled(wizard.getPreviousStep() != null);
        finish.setEnabled(enableFinish);
        JRootPane root = next.getRootPane();
        if (root != null)
        {
            if (next.isEnabled())
            {
                root.setDefaultButton(next);
            }
            else if (finish.isEnabled())
            {
                root.setDefaultButton(finish);
            }
            else if (prev.isEnabled())
            {
                root.setDefaultButton(prev);
            }
            else
            {
                root.setDefaultButton(null);
            }
        }
    }

    public void actionPerformed(ActionEvent event)
    {
        // probably an error status
        if (deferredStatus != null)
        {
            deferredResultFinished(event);
            return;
        }
        
        JButton button = (JButton) event.getSource();

        String name = button.getName();
        if (NAME_NEXT.equals(name))
        {
            processNext();
        }
        else if (NAME_PREV.equals(name))
        {
            processPrev();
        }
        else if (NAME_FINISH.equals(name))
        {
            processFinish(event);
        }
        else if (NAME_CANCEL.equals(name))
        {
            processCancel(event, true);
        }
        else if (NAME_CLOSE.equals(name))
        {
            processClose(event);
        }
        // else ignore, we don't know it

        parent.updateProblem();
    }

    void deferredResultFailed(boolean canGoBack)
    {
        if (!canGoBack)
        {
            getCancel().setText(closeString);
        }
        getPrev().setEnabled(true);
        getNext().setEnabled(false);
        getCancel().setEnabled(true);
        getFinish().setEnabled(false);

        if (NAME_CLOSE.equals(deferredStatus))
        {
            // no action
        }
        else
        {
            deferredStatus = DEFERRED_FAILED + deferredStatus;
        }
        
    }

    void deferredResultFinished(Object o)
    {
        String name = deferredStatus;
        deferredStatus = null;
        
        if (name.startsWith(DEFERRED_FAILED))
        {
            // Cancel clicked after a deferred failure
            if (o instanceof ActionEvent)
            {
                JButton button = (JButton) ((ActionEvent) o).getSource();
                name = button.getName();
                if (NAME_CANCEL.equals(name))
                {
                    processCancel(o instanceof ActionEvent ? (ActionEvent) o
                            : null, false);
                    return;
                }
            }
            // in failed state, so we always reload the current step's screen
            String currentStep = parent.getCurrentStep();
            parent.navigateTo(currentStep);
            return;
        }
        
        if (NAME_NEXT.equals(name))
        {
            processNextProceed(o);
        }
        else if (NAME_PREV.equals(name))
        {
            processPrevProceed(o);
        }
        else if (NAME_CANCEL.equals(name))
        {
            processCancel(o instanceof ActionEvent ? (ActionEvent)o
                    : null, false);
        }
        else if (NAME_FINISH.equals(name))
        {
            // allowFinish on the "down" click of the finish button
            processFinishProceed(o);
        }
        else if (NAME_CLOSE.equals(name))
        {
            // the "up" click of the finish button: wizard.finish was a deferred result
            Window dlg = getWindow();
            dlg.setVisible(false);
            dlg.dispose();
        }
        // else ignore, we don't know it

        parent.updateProblem();
    }

    protected void processNext()
    {
        WizardPanel panel = parent.getCurrentWizardPanel();
        Wizard wizard = parent.getWizard();
        MergeMap settings = parent.getSettings();

        WizardPanelNavResult proceed = WizardPanelNavResult.PROCEED;
        if (panel != null)
        {
            String currentStep = parent.getCurrentStep();
            proceed = panel.allowNext(currentStep, settings, wizard);
            if (proceed.isDeferredComputation())
            {
                deferredStatus = NAME_NEXT;
                // Remove boolean argument  - cholmcc
                // parent.handleDeferredWizardResult(proceed, false);
                parent.handleDeferredWizardResult(proceed);
                return;
            }
        }

        processNextProceed (proceed);
    }

    protected void processNextProceed(Object result)
    {
        Wizard wizard = parent.getWizard();
        MergeMap settings = parent.getSettings();

        if (WizardPanelNavResult.REMAIN_ON_PAGE.equals(result))
        {
            // leave current panel displayed, assume problem is being shown
            return;
        }
        // ignore other results
        
        String nextId = wizard.getNextStep();
        settings.push(nextId);
        parent.navigateTo(nextId);
        parent.setInSummary(false);
    }

    protected void processPrev()
    {
        WizardPanel panel = parent.getCurrentWizardPanel();
        Wizard wizard = parent.getWizard();
        MergeMap settings = parent.getSettings();

        WizardPanelNavResult proceed = WizardPanelNavResult.PROCEED;
        if (panel != null)
        {
            String currentStep = parent.getCurrentStep();
            proceed = panel.allowBack(currentStep, settings, wizard);
            if (proceed.isDeferredComputation())
            {
                deferredStatus = NAME_PREV;
                // Remove boolean arg - cholmcc
                //parent.handleDeferredWizardResult(proceed,false);
                parent.handleDeferredWizardResult(proceed);
                return;
            }
        }

        processPrevProceed (proceed);
    }

    protected void processPrevProceed(Object result)
    {
        Wizard wizard = parent.getWizard();
        MergeMap settings = parent.getSettings();

        if (WizardPanelNavResult.REMAIN_ON_PAGE.equals(result))
        {
            // leave current panel displayed, assume problem is being shown
            return;
        }
        // ignore other results

        String prevId = wizard.getPreviousStep();
        settings.popAndCalve();
        // Remove the call 
        // parent.abortDeferredResult();
        parent.navigateTo(prevId);
        parent.setInSummary(false);
    }

    protected void processFinish(ActionEvent event)
    {
        WizardPanel panel = parent.getCurrentWizardPanel();
        Wizard wizard = parent.getWizard();
        MergeMap settings = parent.getSettings();

        WizardPanelNavResult proceed = WizardPanelNavResult.PROCEED;
        if (panel != null)
        {
            String currentStep = parent.getCurrentStep();
            proceed = panel.allowFinish(currentStep, settings, wizard);
            if (proceed.isDeferredComputation())
            {
                deferredStatus = NAME_FINISH;
                // Remove boolean arg - cholmcc
                // parent.handleDeferredWizardResult((DeferredWizardResult) proceed,false);
                parent.handleDeferredWizardResult((DeferredWizardResult) proceed);
                return;
            }
        }

        processFinishProceed (proceed);
    }

    
    protected void processFinishProceed(Object result)
    {
        Wizard wizard = parent.getWizard();
        MergeMap settings = parent.getSettings();

        if (WizardPanelNavResult.REMAIN_ON_PAGE.equals(result))
        {
            // leave current panel displayed, assume problem is being shown
            return;
        }
        try
        {
            Object o = wizard.finish(settings);
            // System.err.println("WIZARD FINISH GOT ME A " + o);

            boolean closeWindow = true;

            if (o instanceof DeferredWizardResult)
            {
                final DeferredWizardResult r = (DeferredWizardResult) o;
                finish.setEnabled(false);
                cancel.setEnabled(r.canAbort());
                prev.setEnabled(false);
                next.setEnabled(false);

                // the button still says "cancel"
                deferredStatus = NAME_CANCEL;
                // deferredStatus = NAME_CLOSE;
                // Remove boolean arg - cholmcc
                // parent.handleDeferredWizardResult(r, true);
                parent.handleDeferredWizardResult(r);

                closeWindow = false;
            }
            else if (o instanceof Summary)
            {
                parent.handleSummary((Summary) o);
                parent.setWizardResult(((Summary) o).getResult());
                // setSummaryShowingMode will be called
                // need to share code with NavProgress.finished code path
                closeWindow = false;
            }
            else
            {
                parent.setWizardResult(o);
            }

            if (closeWindow)
            {
                // do cancel processing as well
                processCancel(null, false);
            }
        }
        catch (WizardException we)
        {
            if (!suppressMessageDialog) {
                JOptionPane.showMessageDialog(next, we.getLocalizedMessage());
            }
            String id = we.getStepToReturnTo();
            String curr = settings.currID();
            try
            {
                while (id != null && !id.equals(curr))
                {
                    curr = settings.popAndCalve();
                }
                settings.push(id);
                parent.navigateTo(id);
                return;
            }
            catch (NoSuchElementException ex)
            {
                IllegalStateException e = new IllegalStateException("Exception " + // NOI18N
                    "said to return to " + id + " but no such " + // NOI18N
                    "step found"); // NOI18N
                e.initCause(ex);
                throw e;
            }
        }
    }

    protected void processCancel(ActionEvent event, boolean reallyCancel)
    {
    	
        DeferredWizardResult deferredResult = parent.getDeferredResult();
        if (deferredResult != null && deferredResult.canAbort())
        {
            deferredResult.abort();
        }
        Wizard wizard = parent.getWizard();
        MergeMap settings = parent.getSettings();
        
        // System.err.println("ProcessCancel " + reallyCancel + " receiver " + parent.receiver);
        boolean closeWindow = false;
        
        if (reallyCancel && parent.cancel()) 
        {
            // System.err.println("DO CANCEL");
            logger.fine("calling wizard cancel method on " + wizard);
            wizard.cancel (settings);
            return;
        }
        
        closeWindow = reallyCancel ? wizard.cancel(settings) : parent.receiver == null;

        // if we have the event (allowFinish was not deferred) then be very sure to close the proper dialog
        if (closeWindow) {
            Window win = event != null ? (Window) ((JComponent) event.getSource()).getTopLevelAncestor()
                : getWindow();
            win.setVisible(false);
            win.dispose();
        }
    }

    /**
     * Assigns a handler used to carry out the close action.
     * @param l ActionListener to be invoked when the wizard is to be closed.
     * @return the handler replaced by this method invocation
     */
    public ActionListener setCloseHandler(final ActionListener l) {
    	final ActionListener old = closeHandler;
    	closeHandler = l;
    	return old;
    }
    

    protected void processClose(ActionEvent event) {
	closeHandler.actionPerformed(event);
    }

    void updateButtons()
    {
        Wizard wizard = parent.getWizard();
        if (!wizard.isBusy())
        {
            configureNavigationButtons(wizard, prev, next, finish);
        }
    }

    void setSummaryShowingMode ()
    {
        next.setEnabled(false);
        prev.setEnabled(false);
        cancel.setEnabled(true);
        finish.setEnabled(false);
        if (window != null && parent.receiver == null && window instanceof JDialog) {
            ((JDialog) window).getRootPane().setDefaultButton(cancel);
        }

        cancel.setText(closeString); // NOI18N
        cancel.setMnemonic(NbBridge.getString("org/netbeans/api/wizard/Bundle", // NOI18N
                                              WizardDisplayer.class, "Close_mnemonic").charAt(0)); // NOI18N
        cancel.setName(NAME_CLOSE);
        deferredStatus = null;  // ?? should summary be different
    }
    
    void setWindow(Window dlg)
    {
        this.window = dlg;
    }

    public JPanel getButtons()
    {
        return buttons;
    }

    public JButton getCancel()
    {
        return cancel;
    }

    public String getCloseString()
    {
        return closeString;
    }

    public Window getWindow()
    {
        return window;
    }

    public JButton getFinish()
    {
        return finish;
    }

    public JButton getHelp()
    {
        return help;
    }

    public JButton getNext()
    {
        return next;
    }

    public WizardDisplayerImpl getParent()
    {
        return parent;
    }

    public JButton getPrev()
    {
        return prev;
    }

    public void initializeNavigation()
    {
        Wizard wizard = parent.getWizard();
        prev.setEnabled(false);
        next.setEnabled(wizard.getNextStep() != null);
        int fwdNavMode = wizard.getForwardNavigationMode();
        WizardDisplayerImpl.checkLegalNavMode(fwdNavMode);

        finish.setEnabled((fwdNavMode & Wizard.MODE_CAN_FINISH) != 0);

        connectListener();
    }

    // -------------------------------------
    /**
     * Listener for wizard changages that affect button state
     */
    class NavWizardObserver implements WizardObserver
    {
        boolean wasBusy = false;

        public void stepsChanged(Wizard wizard)
        {
            // do nothing
        }

        public void navigabilityChanged(Wizard wizard)
        {
            if (wizard.isBusy())
            {
                next.setEnabled(false);
                prev.setEnabled(false);
                finish.setEnabled(false);
                cancel.setEnabled(false);
                parent.getOuterPanel().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                wasBusy = true;
                return;
            }
            else if (wasBusy)
            {
                cancel.setEnabled(true);
                parent.getOuterPanel().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            }
            configureNavigationButtons(wizard, prev, next, finish);

            parent.updateProblem();
        }

        public void selectionChanged(Wizard wizard)
        {
            // do nothing
        }
    }

}
