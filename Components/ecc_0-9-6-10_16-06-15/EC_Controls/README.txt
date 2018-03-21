ECControls requires following change in controls.pp:
these two methods must be virtual:
line ~722  procedure GetSpaceAround(var SpaceAround: TRect); virtual;

line ~724  function GetSpace(Kind: TAnchorKind): Integer; virtual;

This is done in Lazarus trunk 44805. Lazarus 1.4 already includes this patch.
That's all. Without it will package not compile.
No matter if you have stable Lazarus (1.2) or trunk (1.3).
No matter if you have stable FPC (2.6.4) or trunk (2.7.1)
(but there were some patches to fpdoc which are not in 2.6.4,
 so if you want to create html docs via attached script, you need 2.7.1).

Known issues:
=============
TECSwitch has not OnEditingDone event. This is difference to TCheckBox.
Use OnChange or OnExit instead.

TECCheckListBox does not support sorting. TStringList(Items).Sort will not
crash, but it will sort Text only. CheckBoxes and their Captions will be messed.

TECCheckListBox: do not use methods of property Items (Items.Add, Items.Delete
  etc.). Use methods AddItem, DeleteItem etc. instead. It is due to internal
  design of the control (synchronization of Items (list) and States (array)).

TECScheme: "uses" section: FileUtil replaced with LazFileUtils due to
  changes in Lazarus trunk. See CHANGELOG. If you need to install ECScheme
  to Lazarus 1.4.x, revert this change.

TECTabCtrl: can be visually "ugly" or "incorrect" with some desktop themes
  or some Style setting. (KDE4 Breeze; GTk2 and Style = eosThemedPanel).

ImageIndex property editors (in OI) is not possible for Collections.
  That's because TCollectionItem does not know its owner, which has
  owner, which has Images: TCustomImageList.



