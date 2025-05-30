{**********************************************************************
                PilotLogic Software House
  
 Package pl_Cindy
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)
***********************************************************************}

{   Component(s):
    TcyProgressionPanel

    Description:
    Run-time Progression panel component

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
    
unit cyProgressionPanel;

{$MODE Delphi}

interface

uses Classes, cyCustomProgressionPanel;

type
  TcyProgressionPanel = class(TcyCustomProgressionPanel)
  private
  protected
  public
    property State;
  published
    property Alignment;
    property Autosize;
    property BorderWidth;
    property ButtonCancel;
    property ButtonCancelCaption;
    property Caption;
    property Degrade;
    property FlashWindow;
    property Font;
    property Height;
    property Glyph;
    property GlyphAlign;
    property Layout;
    property Gauge;
    property GaugeMax;
    property GaugeMin;
    // property State
    property Width;
    property OnOpen;
    property OnClose;
    property OnCancelButtonClick;
  end;


implementation

end.
