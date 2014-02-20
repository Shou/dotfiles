# -*- coding: utf-8 -*-
#
# Copyright (c) 2009 by xt <xt@bash.no>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#
# (this script requires WeeChat 0.3.0 or newer)
#
# History:
# 2012-10-13, Shou <x@shou.io>
#   version 0.6: this is a shit
# 2011-07-17, Sébastien Helleu <flashcode@flashtux.org>
#   version 0.5: allow empty value for pairs or words
# 2011-02-01, xt
#   version 0.4: improve regexp for word replacement
# 2010-11-26, xt <xt@bash.no>
#   version 0.3: don't replace in /set commands
# 2009-10-27, xt <xt@bash.no>
#   version 0.2: also replace on words
# 2009-10-22, xt <xt@bash.no>
#   version 0.1: initial release

import weechat as w
import re
from subprocess import Popen, PIPE

SCRIPT_NAME    = "text_replace"
SCRIPT_AUTHOR  = "xt <xt@bash.no>"
SCRIPT_VERSION = "0.6"
SCRIPT_LICENSE = "GPL3"
SCRIPT_DESC    = "Replaces text you write with replacement text"

# script options
settings = {
        'replacement_pairs': '(:=:)|):=:(',   # pairs separated by | orig text and replacement separated by =
        'replacement_words': 'hhe=heh',       # words separated by | orig text and replacement separated by =
}

blacklist = False
chanlist = [] # Where channels are put for replaced words.
def allowed(chan):
    b = chan in chanlist
    if blacklist: return not b
    else: return b

# hsplit :: String -> (Char -> Bool) -> [String]
def breaks(s, f):
    arr = [""]
    n = 0
    b = False
    for c in s:
        if f(c) and not b:
            n += 1
            arr += [c]
            b = True
        elif f(c) and b:
            t = arr.pop()
            arr += [t + c]
        elif b:
            n += 1
            arr += [c]
            b = False
        else:
            t = arr.pop()
            arr += [t + c]
    return arr

# notAlphabet :: Char -> Bool
def notAlphabet(x): return not (x in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

rs = {
    "#dunno": "╮(─▽─)╭",
    "#shrug": "┐('～`；)┌",
    "#kshrug": "╮(.❛ ᴗ ❛.)╭",
    "#mad": "ヽ(ｏ`皿′ｏ)ﾉ",
    "#happy": "ヽ(゜∀゜)ノ",
    "#sad": "(´；ω；`)",
    "#dep": "（ ´,_ゝ`)",
    "#haa": "(*´Д`)ﾊｧﾊｧ",
    "#evil": "( ﾟ∀ﾟ)ｱﾊﾊ八八ﾉヽﾉヽﾉヽﾉ ＼ / ＼/ ＼",
    "#dance": "♪┏(・o･)┛♪┗ ( ･o･) ┓♪┏ ( ) ┛♪┗ (･o･ ) ┓♪┏(･o･)┛♪",
    "#disco": "\x0309♪┏(・o･)┛\x0310♪┗ ( ･o･) ┓\x0304♪┏ ( ) ┛\x0313♪┗ (･o･ ) ┓\x0308♪┏(･o･)┛♪\x03",
    "#run2": "ε=ε=ε=┌(;*´Д`)ﾉ",
    "#run1": "ε＝ε＝ε＝┌( ﾟ∀ﾟ)┘",
    "#chu1": "(*^3^)/~☆",
    "#jizz": "( ' ヮ')ノ.・ﾟ*｡・.・ﾟ*｡・.・ﾟ*｡・ヽ(ﾟДﾟ,,)ノ",
    "#comp": "□＿ヾ(･ω･ )カタカタ",
    "#nade": "(╥﹏╥)ヾ(･ω･ )",
    "#eh": "┐(ﾟ∀ﾟ)┌",
    "#yay": "ヽ(' ▽' )ノ !",
    "#slap1": "( ´Д｀)ﾉ)`ν゜)",
    "#slap2": "┐(ﾟ∀ﾟ)ﾉ)`ν゜)",
    "#omgz": "（　ﾟДﾟ） !!!",
    "#ah.": "（´∀｀）",
    "#what1": "（　´_ゝ`）",
    "#what2": "（´<_｀ 　）",
    "#shock1": "Σ(゜д゜;)",
    "#shock2": "ｶﾞ━━Σ(ﾟДﾟ|||)━━ﾝ!!",
    "#shock3": "ｶﾞ━━━━━━━Σ(ﾟ□ﾟ*川━━━━━━━━ﾝ!",
    "#omg": "(*´Д`)",
    "#smile": "(ﾟ∀ﾟ)",
    "#heart": "04♥ ω 04♥",
    "#rabu": "04♥ ◡ 04♥",
    "#awesome": "( ﾟ ヮﾟ)",
    "#herp": "（　｀ー´）",
    "#wtf1": "щ(ﾟДﾟщ)",
    "#wtf2": "☜(ﾟДﾟщ)",
    "#ew2": "(*≧д≦)",
    "#cry1": "ヽ(´□｀。)ﾉ",
    "#cry2": "。゜(゜´Д｀゜)゜。",
    "#cry3": "11｡･ﾟ･(ﾉД｀)11･ﾟ･｡",
    "#ohayou": "~~~ヾ(＾∇＾)おはよー♪",
    "#konbanwa": "( ﾟ▽ﾟ)/ｺﾝﾊﾞﾝﾊ",
    "#seeya": "(ヾ(´･ω･｀)いってらっちゃ",
    "#back": "( ´ ▽ ` )ﾉ ﾀﾀﾞｲﾏｧ",
    "#wback": "ｵｶｴﾘ♪(ﾉ´∀｀*)ﾉ",
    "#kik": "Σ(ﾟ∀´(┗┐ヽ(･∀･ )ﾉ",
    "#irrashai": "＼(＾▽＾*)いらっしゃ～いっ(*＾▽＾)／",
    "#welcome": "(☞ﾟ∀ﾟ)☞4★゜・。7。・゜゜・。8。・゜☆゜・。9。・゜゜・。 WELCOME 11。・゜゜・。12。・゜☆゜・。13。・゜゜・。6。・゜",
    "#love": "ｷｬｰ(>ω<＊)ﾉﾉ♥",
    "#congrats": "オメデトウ　( ^ _ ^)∠☆PAN！",
    "#doki": "ﾄﾞｷ!（'O'（ｰ^*）chu♪",
    "#chu2": "☆⌒ヽ(*'､^*)chu",
    "#eto": "Σ(￣∀￣;|||･･･",
    "#bonk": "(>_<。)＼~ イタタ",
    "#hug": "(っ´ω｀)っhug?~",
    "#wink": "( ´・‿- ) ~ 4♥",
    "#beam1": "(ノﾟοﾟ)ノﾐ4★゜・。7。・゜゜・。8。・゜☆゜・。9。・゜゜・。11。・゜゜・。12。・゜☆゜・。13。・゜゜・。6。・゜",
    "#beamu": "(☞ﾟ∀ﾟ)☞4★゜・。7。・゜゜・。8。・゜☆゜・。9。・゜゜・。11。・゜゜・。12。・゜☆゜・。13。・゜゜・。6。・゜",
    "#beamx": "(ノﾟοﾟ)ノﾐ13★4ﾊ7ﾊ8八9八11ﾉヽ12ﾉヽ13ﾉヽ6ﾉ ＼ 4/ ＼7/ ＼☆゜・。8。・゜゜・。9。・゜☆゜・。11。・゜゜・。12。・゜",
    "#heehee^": "（⌒◡⌒）",
    "#flip1": "(ﾉ `Д´)ﾉ ~┻━┻",
    "#aah1": "（ ￣ヮ￣）～♪",
    "#aah2": "（ ￣ヮ￣）～♫",
    "#eek": "(つд⊂)",
    "#kita": "ｷﾀ━━━━━（゜∀゜）━━━━━!!!!!",
    "#yaaay": "ヽ（＞ヮ＜）ノ",
    "#huh2": "( 　ﾟ _ゝﾟ) .....",
    "#huh3": "( 　;_ゝ;)",
    "#kya1": "ლ(╹◡╹ლ)",
    "#kya2": "(ノﾟ∀ﾟ)ノ",
    "#magic": "ABRA KADABRA! ヽ（ ﾟヮﾟ）ﾉ.・ﾟ*｡・+☆┳━┳",
    "#weeaboo": "＼(＾▽＾*)YAY BEING A WEEABOO(*＾▽＾)／",
    "#punch": "( ゜▽゜)=◯)`ν゜)",
    "#longhuh": "(　´_________________ゝ_________________｀)",
    "#whoa": "(　；∀；)",
    "#blue": "v11:",
    "#no": "（　´_ゝ`）ﾉ)`ν゜)",
    "#fff": "(ノ ゜Д゜)ノ",
    "#wow": "┐(ﾟ∀ﾟ)┌....",
    "#yatta": "ヽ（＞ヮ＜）ノやった～",
    "#hur": "┐（´∀｀）┌",
    "#poke": "o(´・ω・)つ)´_ゝ｀)",
    "#gface": "( ≖‿≖)",
    "#mikuface": "11/06°11(  ≖‿≖11)06°11\\",
    "#ni1": "（　≖‿≖）",
    "#ni2": "（  ≖‿‿≖）",
    "#ni3": "（  4≖‿‿4≖）",
    "#yes": "（　´‿ゝ｀）",
    "#wtff": "(屮゜Д゜)屮",
    "#:3c": "っℇ:",
    "#ublush": "（＞4／／／／＜）",
    "#sageru": "11SAGERU0 All you 4WANT0 but you can 2NEVER0 crush my 4~JAPANESE12SPIRIT~",
    "#goddamn": "........... （　´_ゝ`）",
    "#flip2": "(ﾉ `Д´)ﾉ ~┻━┻)ﾟДﾟ)彡☆",
    "#woo": "ヽ( ﾟ ヮﾟ)ノ",
    "#heh": "┐(￣ー￣)┌",
    "#omggz": "ヾ(●ﾟワﾟ●)ﾉ゛",
    "#hi51": "(　｀ー´)八(｀ー´　) ＨＩ５",
    "#hibro": "(　｀ー´)ﾉ DON'T LEAVE ME HANGING BRO",
    "#5bro": "ヽ(｀ー´　) ＨＩ５",
    "#kuu1": "(/ω＼)",
    "#kuu2": "(｡ﾉω＼｡)",
    "#durr": "（ ｡∀ﾟ ）",
    "#kuu3": "(＞人＜;)",
    "#tea": "(´ω`)っ旦~",
    "#sdf": "┐( ﾟдﾟ)┌",
    "#star": "(ﾉ ﾟ◡◡ﾟ)ﾉ☆",
    "#star2": "(ﾉ ﾟ◡◡ﾟ)ﾉ4☆7☆8☆9☆11☆13☆",
    "#sdl": "(°◡°♡)",
    "#sdc": "( ﾟ◡ﾟ)",
    "#cflower": "(╹◡╹✿)",
    "#sflower": "(◡ ‿ ◡ ✿)",
    "#hflower": "(✿ ◠ ‿ ◠)",
    "#sd2": "( ﾟ◡◡ﾟ)",
    "#sd3": "( 4ﾟ◡◡4ﾟ)",
    "#sd4": "( 4ﾟ◡◡4ﾟ)",
    "#daaamn": "2(((((((12(((((((((((((((11(((((((((((((4゜△゜5;11)))))))))))12)))))))))))))))))))2)))))))))))))))))",
    "#brofist": "し(*･∀･)／4♡＼(･∀･*) ＢＲＯ　ＦＩＳＴＵ",
    "#bro5": "(　｀ー´)／4♥＼(･∀･*) ＢＲＯＨＩＦＩＳＴＵ５",
    "#wave1": "(・∀・)ノ゛",
    "#wave2": "゛ヽ(･∀･)",
    "#d1": "B :^J",
    "#d2": "B:^J",
    "#d3": "B^J",
    "#d4": "deal w/ it",
    "#wtff2": "(屮゜Д゜)☞",
    "#wave3": "( ﾟ ヮﾟ)ノ゛",
    "#wave4": "゛ヽ(ﾟ ヮﾟ )",
    "#heehee~": "ヽ(･∀･ )ﾉ",
    "#(:3)": "(・ω・)",
    "#perky": "(｀・ω ・´)",
    "#,": "(´ω`)",
    "#,,": "(´ω`)フゥ～",
    "#bwtf": "щ(ﾟДﾟщ)",
    "#bwtff": "(屮゜Д゜)屮",
    "#geez": "(屮゜Д゜)屮 щ(ﾟДﾟщ)",
    "#durr2": "(。‿°)",
    "#:33": "4・ ω 4・",
    "#bluh": "┐(ﾟ―ﾟ)┌",
    "#raise": "ヽ(´ー｀)ﾉ",
    "#ngh": "(゜△゜;)",
    "#c:": "（　´∀｀）",
    "#fa": "(ﾉ 。々°)ﾉ",
    "#fab1": "ヽ( ＊￣▽￣)ノ4★゜・。7。・゜゜・。8。・゜☆6Ｆ13Ａ6Ｂ13Ｕ6Ｌ13Ｏ6Ｕ13Ｓ6～9☆゜・。11。・゜゜・。2。・゜★",
    "#fab2": "ヽ( ＊￣▽￣)ノ☆ＦＡＢＵＬＯＵＳ～☆",
    "#gay5": "♡(✿ˇ◡ˇ)人(ˇ◡ˇ✿)♡ ",
    "#kira": "(づ4｡◕‿‿◕4｡)づ4。13。4・13゜4゜13・4。13。4・13゜4❤",
    "#rape": "(✧≖‿ゝ≖)",
    "#hi52": "(=゜ω゜)人(゜ω゜=)",
    "#wblush": "(4///ω4///)",
    "#kyu": "（＞ ω ＜）13❤",
    "#lblush": "(4////////ω4////////)",
    "#bgeez": "(屮゜Д゜)屮 щ(ﾟДﾟщ)",
    "#ji": "( ¬‿¬)",
    "#punch2": "キタ━━━(Д゜(○=(゜∀゜)=○)Д゜)━━━!!",
    "#aha1": "ァ '`,、'`,、'`,、'`,、(´▽`) '`,、'`,、'`,、'`,、'`,",
    "#ah!": "(。-ω-)zzz. . . (。ﾟωﾟ) ﾊｯ!",
    "#ew1": "Σ(｀д｀ﾉ)ﾉ ﾇｵｫ!!",
    "#grrr": "（\"｀д´）",
    "#ku2": "ｷｬｯ(/ω＼*))((*／ωヽ)ｷｬｯ",
    "#omgg": "o(＞ω＜)o",
    "#cry4": "（；へ；）",
    "#nnn": "（￣、￣＠）...",
    "#ok~": "ヽ( ´ ▽ ` )ﾉ ﾊｰｰｰｰｰｰｰｲ",
    "#fyeah": "8★12｡11･4❤2ﾟ11･FUCKヽ( ≧ ▽≦)人(･ิ◡ ･ิ )ﾉYEAH11･2ﾟ4❤11･12｡8★",
    "#aha2": "( ﾟ∀ﾟ)ｱﾊﾊ八八ﾉヽﾉヽﾉヽﾉ ＼ / ＼/ ＼ ＼ /＼ / ＼ ﾉヽﾉヽﾉヽﾉ八八ﾊﾊｱ)`ν°)",
    "#eh1": "┐( ｡∀ﾟ )┌",
    "#sobb": "(　; ‿ゝ; )",
    "#hhaa": "(;￣▽￣)ノ",
    "#nii": "(≖⁀≖ )",
    "#manlytear": "(　´,_ゝ｀)",
    "#ehh22": "┐(　´〰`)┌ ",
    "#pout": "（´°へ°`）",
    "#mou": "(★´3`)ノ★*♪。☆*★*♪。☆*★*♪。☆*★*♪。☆*★*♪。☆*★*♪。☆*",
    "#bku2": "ｷｬｯ(/ω＼*))((*／ωヽ)ｷｬｯ",
    "#sleep": "(。-ω-)zzz. . .",
    "#slepy": "(≖ _ ≖)",
    "#oyasumi": "(。-ω-)zzz おやすみ. . .",
    "#swizard": "11゜・。13。・゜9゜・。。・゜7☆ヽ(｀ー´)ﾉ 7ｱﾊﾊ9八八ﾉヽ13ﾉヽﾉヽﾉ ＼11 / ＼/ ＼",
    "#hwizard": "11゜・。13。・゜9゜・。。・゜7☆ヽ( ゜∀゜)ﾉ 7ｱﾊﾊ9八八ﾉヽ13ﾉヽﾉヽﾉ ＼11 / ＼/ ＼",
    "#proud": "(▰˘◡˘▰)",
    "#kuma": "ʕ•ᴥ•ʔ",
    "#:c": "( ﾟ∩ﾟ)",
    "#aaaa": "휴Д휴",
    "#wellfuck": "(ﾟﾍﾟ )",
    "#tables": "4┳━┳ ┻━┻ ┳━┳ ┻━┻ ┳━┳ ┻━┻ ┳━┳ ┻━┻~7~8~ヽ(4'～4`；)ﾉ 8~7~4~┻━┻ ┳━┳ ┻━┻ ┳━┳ ┻━┻ ┳━┳ ┻━┻ ┳━┳",
    "#m:33": "(✿ﾟ◡ﾟ) ～13❤4❤13❤4❤",
    "#p:33": "(✿ ｀・ω・´) ～13❤4❤13❤4❤",
    "#w:33": "(✿ ・ω・) ～13❤4❤13❤4❤",
    "#where": "(ﾟДﾟ〓ﾟДﾟ)",
    "#pout2": "(≧ᵔ≦)",
    "#c8": "1,1（　4,1°1,1‿‿ゝ4,1°1,1）",
    "#qw": "━━━━━━( ´∀`)･ω･) ﾟДﾟ)ﾟ∀ﾟ)･∀･)￣ｰ￣)´_ゝ`)-_)´Д｀)ﾟｰﾟ)━━━━━━!!!!",
    "#why": "(ノ ゜Д゜)ノ",
    "#ffff": "(ノ 4゜Д4゜)ノ",
    "#FIGHTO": "(　`Д´)ﾉ)`ν゜)  1,8/!\ 04INTERNET FIGHT01/!\  ヽ(`Д´)ﾉ (゜△゜;)",
    "#eh3": "（￣～￣;）",
    "#fnope": "ε＝ε＝ε＝┌(휴Д휴; )┘ NOPE NOPE NOPE",
    "#:<": "( ' ^ ') !!",
    "#want": "13ｵｫ!!( 8✧◡◡8✧).・ﾟ*｡・+8☆13❤",
    "#hehe": "（　°‿ゝ°）",
    "#hi53": "(　｀ー´)八(´ー｀)八(｀ー´　) ＨＩ５",
    "#easy": "ヽ(´ ▽ `)ノ",
    "#->": "08★ 06→",
    "#frustrate": "U FRUSTRATED U FRUSTRATED BRO U SO MAD WHY ARE YOU SO MAAAAD I CAN POST ANYTHING I WANT THAT IS HOW IT SAYS IN THE RULES I DONT CARE ABOUT YOUR FAGGOTRY RULES Y SO MAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD",
    "#angry": "┌(；`～,)┐",
    "#didyoumean:": "04Did you mean: 12",
    "(tm)": "™",
    "#kamina": "◥▶̸̱◀◤"
}

brit = {
    "amazing": "smashing",
    "Amazing": "Smashing",
    "crazy": "mental",
    "Crazy": "Mental",
    "amazed": "flabbergasted",
    "Amazed": "Flabbergasted",
    "an idiot": "a tosser",
    "An idiot": "Tosser",
    "idiot": "tosser",
    "Idiot": "Tosser",
    "baka": "tosser",
    "Baka": "Tosser",
    "awesome": "bee's knees",
    "Awesome": "Bee's knees",
    "tired": "knackered",
    "Tired": "Knackered",
    "very tired": "knackered",
    "Very tired": "Knackered",
    "great": "brilliant",
    "Great": "Brilliant",
    "ass": "arse",
    "Ass": "Arse",
    "naked": "starkers",
    "Naked": "Starkers",
    "lewd": "lascivious",
    "Lewd": "Lascivious",
    "go away": "bugger off",
    "Go away": "Bugger off",
    "thanks": "cheers",
    "Thanks": "Cheers",
    "pleased": "chuffed",
    "Pleased": "Chuffed",
    "cigarette": "fag",
    "Cigarette": "Fag",
    "drunk": "plastered",
    "Drunk": "Plastered",
    "breasts": "knockers",
    "Breasts": "Knockers",
    "dude": "mate",
    "Dude": "Mate",
    "steal": "nick",
    "Steal": "Nick",
    "stolen": "nicked",
    "stolen": "nicked",
    "stole": "nicked",
    "Stole": "Nicked",
    "food": "nosh",
    "Food": "Nosh",
    "kiss": "snog",
    "Kiss": "Snog",
    "how are you": "all right",
    "How are you": "All right",
    "obvious": "blatant",
    "Obvious": "Blatant",
    "butt": "bum",
    "Butt": "Bum",
    "boring": "dull",
    "Boring": "Dull",
    "dollar": "quid",
    "Dollar": "Quid",
    "dollars": "quid",
    "Dollars": "Quid",
    "terrible": "bollocks",
    "Terrible": "Bollocks",
    "bad": "bollocks",
    "Bad": "Bollocks",
    "television": "telly",
    "Television": "Telly",
    "absurd": "preposterous",
    "Absurd": "Preposterous",
    "ridiculous": "preposterous",
    "Ridiculous": "Preposterous",
    "damn it": "bloody hell",
    "Damn it": "Bloody hell",
    "damn": "ruddy",
    "Damn": "Ruddy"
}

cute = {
    "guy": "girl",
    "Guy": "Girl",
    "GUY": "GIRL",
    "dude": "girl",
    "Dude": "Girl",
    "DUDE": "GIRL",
    "guys": "girls",
    "Guys": "Girls",
    "GUYS": "GIRLS",
    "he": "she",
    "He": "She",
    "HE": "SHE",
    "his": "her",
    "His": "Her",
    "HIS": "HER",
    "him": "her",
    "Him": "Her",
    "HIM": "HER",
    "fap": "schlick",
    "Fap": "Schlick",
    "FAP": "SCHLICK",
    "fapped": "schlicked",
    "Fapped": "Schlicked",
    "FAPPED": "SCHLICKED",
    "fapping": "schlicking",
    "Fapping": "Schlicking",
    "FAPPING": "SCHLICKING",
    "man": "girl",
    "Man": "Girl",
    "MAN": "GIRL",
    "men": "girls",
    "Men": "Girls",
    "MEN": "GIRLS",
    "fuck": "hold hands",
    "Fuck": "Hold hands",
    "FUCK": "HOLD HANDS",
    "fucker": "hand holder",
    "Fucker": "Hand holder",
    "FUCKER": "HAND HOLDER",
    "fug": "hold hands",
    "Fug": "Hold hands",
    "FUG": "HOLD HANDS",
    "fucking": "holding hands",
    "Fucking": "Holding hands",
    "FUCKING": "HOLDING HANDS",
    "fucked": "held hands",
    "Fucked": "Held hands",
    "FUCKED": "HELD HANDS",
    "ass": "butt",
    "Ass": "Butt",
    "ASS": "BUTT",
    "asshole": "butt",
    "Asshole": "Butt",
    "ASSHOLE": "BUTT",
    "shit": "cute butt",
    "Shit": "Cute butt",
    "SHIT": "CUTE BUTT",
    "shitting": "cute butting",
    "Shitting": "Cute butting",
    "SHITTING": "CUTE BUTTING",
    "waifu": "imaginary girlfriend",
    "Waifu": "Imaginary girlfriend",
    "WAIFU": "IMAGINARY GIRLFRIEND",
    "motherfucker": "flat chested girl",
    "Motherfucker": "Flat chested girl",
    "MOTHERFUCKER": "FLAT CHESTED GIRL",
    "cunt": "p-pussy",
    "Cunt": "P-pussy",
    "CUNT": "P-PUSSY",
    "cunts": "p-pussies",
    "Cunts": "P-pussies",
    "CUNTS": "P-PUSSIES",
    "pussy": "p-pussy",
    "Pussy": "P-pussy",
    "PUSSY": "P-PUSSY",
    "penis": "p-penis",
    "Penis": "P-penis",
    "PENIS": "P-PENIS",
    "cock": "p-penis",
    "Cock": "P-penis",
    "COCK": "P-PENIS",
    "damn": "kiss",
    "Damn": "Kiss",
    "DAMN": "KISS",
    "hell": "Gensokyo",
    "Hell": "Gensokyo",
    "HELL": "GENSOKYO",
    "suck": "hug",
    "Suck": "Hug",
    "SUCK": "HUG",
    "sucked": "hugged",
    "Sucked": "Hugged",
    "SUCKED": "HUGGED",
    "sucking": "hugging",
    "Sucking": "Hugging",
    "SUCKING": "HUGGING",
    "coming": "cumming",
    "Coming": "Cumming",
    "COMING": "CUMMING"
}

ss = brit



if w.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
                    SCRIPT_DESC, "", ""):
    for option, default_value in settings.iteritems():
        if not w.config_is_set_plugin(option):
            w.config_set_plugin(option, default_value)

    # Hooks we want to hook
    hook_command_run = {
        "input" : ("/input return",  "command_run_input"),
    }
    # Hook all hooks !
    for hook, value in hook_command_run.iteritems():
        w.hook_command_run(value[0], value[1], "")


def command_run_input(data, buffer, command):
    """ Function called when a command "/input xxxx" is run """
    if command == "/input return": # As in enter was pressed.

        # Get input contents
        input_s = w.buffer_get_string(buffer, 'input')

        # Get channel
        channel = w.buffer_get_string(buffer, 'short_name')

        # Skip modification of settings
        if input_s.startswith('/set '):
            return w.WEECHAT_RC_OK

        elif not re.match("^(https?://|>|/(?!/|stutter|msg|me))", input_s, re.I):
            for r in rs:
                input_s = input_s.replace(r, rs[r])
            if allowed(channel):
                result = ""
                for wo in breaks(input_s, notAlphabet):
                    if wo in ss.keys():
                        wo = ss[wo]
                    result += wo
                input_s = result

        # Iterate transformation pairs
        #for replace_item in w.config_get_plugin('replacement_pairs').split('|'):
        #    if replace_item:
        #        orig, replaced = replace_item.split('=')
        #        input_s = input_s.replace(orig, replaced)
        # Iterate words
        #for replace_item in w.config_get_plugin('replacement_words').split('|'):
        #    if replace_item:
        #        orig, replaced = replace_item.split('=')
        #        # Search for whitespace+word+whitespace and replace the word
        #        input_s = re.sub('(\s+|^)%s(\s+|$)' %orig, '\\1%s\\2' %replaced, input_s)

        # Spit it out
        w.buffer_set(buffer, 'input', input_s)
    return w.WEECHAT_RC_OK
