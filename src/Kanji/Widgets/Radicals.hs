{-|
Module      : Kanji.Widgets.Radicals
Copyright   : (c) 2023 David Wilson
License     : BSD-3-Clause
Maintainer  : David Wilson

A simple widget to select Radicals.

-}

module Kanji.Widgets.Radicals
  ( radicalGrid
  , RadEvent
  ) where

import Data.List qualified as List

import Monomer

-- help

-- Split a list every n elements
splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs 
  | n <= 1 = map (:[]) xs
  | otherwise = splitEvery' n xs
  where
    splitEvery' n xs
      | (List.null zs) = [ys]
      | otherwise = (ys:(splitEvery' n zs))
      where (ys,zs) = List.splitAt n xs

data RadEvent = InpRad Char deriving (Show, Eq)

-- | A grid that creates events every time a widget is selected.
-- Does not allow you to toggle buttons on/off.
radicalGrid :: (WidgetEvent e) => (RadEvent -> e) -> Int -> WidgetNode s e
radicalGrid f mxCols
  = vgrid (map hgrid radicalRows)
  where 
    mxCols' = max 5 mxCols
    radicalRows = splitEvery mxCols' (radicalNodes f)


-- [bgColor dimGray, padding 10, border 2 black, radius 3]

radicalNodes :: (WidgetEvent e) => (RadEvent -> e) -> [WidgetNode s e]
radicalNodes f = map (`styleBasic` [textFont "Japanese", textSize 12]) 
    [ styleLabel $ label "1"
    , button "一" (f $ InpRad '一')
    , button "｜" (f $ InpRad '｜')
    , button "丶" (f $ InpRad '丶')
    , button "ノ" (f $ InpRad 'ノ')
    , button "乙" (f $ InpRad '乙')
    , button "亅" (f $ InpRad '亅')

    , styleLabel $ label "2"
    , button "二" (f $ InpRad '二')
    , button "亠" (f $ InpRad '亠')
    , button "人" (f $ InpRad '人')
    , button "⺅" (f $ InpRad '⺅')
    , button "𠆢" (f $ InpRad '𠆢')
    , button "儿" (f $ InpRad '儿')
    , button "入" (f $ InpRad '入')
    , button "ハ" (f $ InpRad 'ハ')
    , button "丷" (f $ InpRad '丷')
    , button "冂" (f $ InpRad '冂')
    , button "冖" (f $ InpRad '冖')
    , button "冫" (f $ InpRad '冫')
    , button "几" (f $ InpRad '几')
    , button "凵" (f $ InpRad '凵')
    , button "刀" (f $ InpRad '刀')
    , button "⺉" (f $ InpRad '⺉')
    , button "力" (f $ InpRad '力')
    , button "勹" (f $ InpRad '勹')
    , button "匕" (f $ InpRad '匕')
    , button "匚" (f $ InpRad '匚')
    , button "十" (f $ InpRad '十')
    , button "卜" (f $ InpRad '卜')
    , button "卩" (f $ InpRad '卩')
    , button "厂" (f $ InpRad '厂')
    , button "厶" (f $ InpRad '厶')
    , button "又" (f $ InpRad '又')
    , button "マ" (f $ InpRad 'マ')
    , button "九" (f $ InpRad '九')
    , button "ユ" (f $ InpRad 'ユ')
    , button "乃" (f $ InpRad '乃')
    , button "𠂉" (f $ InpRad '𠂉')
    
    , styleLabel $ label "3"
    , button "⻌" (f $ InpRad '⻌')
    , button "口" (f $ InpRad '口')
    , button "囗" (f $ InpRad '囗')
    , button "土" (f $ InpRad '土')
    , button "士" (f $ InpRad '士')
    , button "夂" (f $ InpRad '夂')
    , button "夕" (f $ InpRad '夕')
    , button "大" (f $ InpRad '大')
    , button "女" (f $ InpRad '女')
    , button "子" (f $ InpRad '子')
    , button "宀" (f $ InpRad '宀')
    , button "寸" (f $ InpRad '寸')
    , button "小" (f $ InpRad '小')
    , button "⺌" (f $ InpRad '⺌')
    , button "尢" (f $ InpRad '尢')
    , button "尸" (f $ InpRad '尸')
    , button "屮" (f $ InpRad '屮')
    , button "山" (f $ InpRad '山')
    , button "川" (f $ InpRad '川')
    , button "巛" (f $ InpRad '巛')
    , button "工" (f $ InpRad '工')
    , button "已" (f $ InpRad '已')
    , button "巾" (f $ InpRad '巾')
    , button "干" (f $ InpRad '干')
    , button "幺" (f $ InpRad '幺')
    , button "广" (f $ InpRad '广')
    , button "廴" (f $ InpRad '廴')
    , button "廾" (f $ InpRad '廾')
    , button "弋" (f $ InpRad '弋')
    , button "弓" (f $ InpRad '弓')
    , button "ヨ" (f $ InpRad 'ヨ')
    , button "彑" (f $ InpRad '彑')
    , button "彡" (f $ InpRad '彡')
    , button "彳" (f $ InpRad '彳')
    , button "⺖" (f $ InpRad '⺖')
    , button "⺘" (f $ InpRad '⺘')
    , button "⺡" (f $ InpRad '⺡')
    , button "⺨" (f $ InpRad '⺨')
    , button "⺾" (f $ InpRad '⺾')
    , button "⻏" (f $ InpRad '⻏')
    , button "⻖" (f $ InpRad '⻖')
    , button "也" (f $ InpRad '也')
    , button "亡" (f $ InpRad '亡')
    , button "及" (f $ InpRad '及')
    , button "久" (f $ InpRad '久')

    , styleLabel $ label "4"
    , button "⺹" (f $ InpRad '⺹')
    , button "心" (f $ InpRad '心')
    , button "戈" (f $ InpRad '戈')
    , button "戸" (f $ InpRad '戸')
    , button "手" (f $ InpRad '手')
    , button "支" (f $ InpRad '支')
    , button "攵" (f $ InpRad '攵')
    , button "文" (f $ InpRad '文')
    , button "斗" (f $ InpRad '斗')
    , button "斤" (f $ InpRad '斤')
    , button "方" (f $ InpRad '方')
    , button "无" (f $ InpRad '无')
    , button "日" (f $ InpRad '日')
    , button "曰" (f $ InpRad '曰')
    , button "月" (f $ InpRad '月')
    , button "木" (f $ InpRad '木')
    , button "欠" (f $ InpRad '欠')
    , button "止" (f $ InpRad '止')
    , button "歹" (f $ InpRad '歹')
    , button "殳" (f $ InpRad '殳')
    , button "比" (f $ InpRad '比')
    , button "毛" (f $ InpRad '毛')
    , button "氏" (f $ InpRad '氏')
    , button "气" (f $ InpRad '气')
    , button "水" (f $ InpRad '水')
    , button "火" (f $ InpRad '火')
    , button "⺣" (f $ InpRad '⺣')
    , button "爪" (f $ InpRad '爪')
    , button "父" (f $ InpRad '父')
    , button "爻" (f $ InpRad '爻')
    , button "爿" (f $ InpRad '爿')
    , button "片" (f $ InpRad '片')
    , button "牛" (f $ InpRad '牛')
    , button "犬" (f $ InpRad '犬')
    , button "⺭" (f $ InpRad '⺭')
    , button "王" (f $ InpRad '王')
    , button "元" (f $ InpRad '元')
    , button "井" (f $ InpRad '井')
    , button "勿" (f $ InpRad '勿')
    , button "尤" (f $ InpRad '尤')
    , button "五" (f $ InpRad '五')
    , button "屯" (f $ InpRad '屯')
    , button "巴" (f $ InpRad '巴')
    , button "毋" (f $ InpRad '毋')

    , styleLabel $ label "5"
    , button "玄" (f $ InpRad '玄')
    , button "瓦" (f $ InpRad '瓦')
    , button "甘" (f $ InpRad '甘')
    , button "生" (f $ InpRad '生')
    , button "用" (f $ InpRad '用')
    , button "田" (f $ InpRad '田')
    , button "疋" (f $ InpRad '疋')
    , button "疒" (f $ InpRad '疒')
    , button "癶" (f $ InpRad '癶')
    , button "白" (f $ InpRad '白')
    , button "皮" (f $ InpRad '皮')
    , button "皿" (f $ InpRad '皿')
    , button "目" (f $ InpRad '目')
    , button "矛" (f $ InpRad '矛')
    , button "矢" (f $ InpRad '矢')
    , button "石" (f $ InpRad '石')
    , button "示" (f $ InpRad '示')
    , button "禸" (f $ InpRad '禸')
    , button "禾" (f $ InpRad '禾')
    , button "穴" (f $ InpRad '穴')
    , button "立" (f $ InpRad '立')
    , button "⻂" (f $ InpRad '⻂')
    , button "世" (f $ InpRad '世')
    , button "巨" (f $ InpRad '巨')
    , button "冊" (f $ InpRad '冊')
    , button "母" (f $ InpRad '母')
    , button "⺲" (f $ InpRad '⺲')
    , button "牙" (f $ InpRad '牙')

    , styleLabel $ label "6"
    , button "瓜" (f $ InpRad '瓜')
    , button "竹" (f $ InpRad '竹')
    , button "米" (f $ InpRad '米')
    , button "糸" (f $ InpRad '糸')
    , button "缶" (f $ InpRad '缶')
    , button "羊" (f $ InpRad '羊')
    , button "羽" (f $ InpRad '羽')
    , button "而" (f $ InpRad '而')
    , button "耒" (f $ InpRad '耒')
    , button "耳" (f $ InpRad '耳')
    , button "聿" (f $ InpRad '聿')
    , button "肉" (f $ InpRad '肉')
    , button "自" (f $ InpRad '自')
    , button "至" (f $ InpRad '至')
    , button "臼" (f $ InpRad '臼')
    , button "舌" (f $ InpRad '舌')
    , button "舟" (f $ InpRad '舟')
    , button "艮" (f $ InpRad '艮')
    , button "色" (f $ InpRad '色')
    , button "虍" (f $ InpRad '虍')
    , button "虫" (f $ InpRad '虫')
    , button "血" (f $ InpRad '血')
    , button "行" (f $ InpRad '行')
    , button "衣" (f $ InpRad '衣')
    , button "西" (f $ InpRad '西')

    , styleLabel $ label "7"
    , button "臣" (f $ InpRad '臣')
    , button "見" (f $ InpRad '見')
    , button "角" (f $ InpRad '角')
    , button "言" (f $ InpRad '言')
    , button "谷" (f $ InpRad '谷')
    , button "豆" (f $ InpRad '豆')
    , button "豕" (f $ InpRad '豕')
    , button "豸" (f $ InpRad '豸')
    , button "貝" (f $ InpRad '貝')
    , button "赤" (f $ InpRad '赤')
    , button "走" (f $ InpRad '走')
    , button "足" (f $ InpRad '足')
    , button "身" (f $ InpRad '身')
    , button "車" (f $ InpRad '車')
    , button "辛" (f $ InpRad '辛')
    , button "辰" (f $ InpRad '辰')
    , button "酉" (f $ InpRad '酉')
    , button "釆" (f $ InpRad '釆')
    , button "里" (f $ InpRad '里')
    , button "舛" (f $ InpRad '舛')
    , button "麦" (f $ InpRad '麦')

    , styleLabel $ label "8"
    , button "金" (f $ InpRad '金')
    , button "長" (f $ InpRad '長')
    , button "門" (f $ InpRad '門')
    , button "隶" (f $ InpRad '隶')
    , button "隹" (f $ InpRad '隹')
    , button "雨" (f $ InpRad '雨')
    , button "青" (f $ InpRad '青')
    , button "非" (f $ InpRad '非')
    , button "奄" (f $ InpRad '奄')
    , button "岡" (f $ InpRad '岡')
    , button "免" (f $ InpRad '免')
    , button "斉" (f $ InpRad '斉')

    , styleLabel $ label "9"
    , button "面" (f $ InpRad '面')
    , button "革" (f $ InpRad '革')
    , button "韭" (f $ InpRad '韭')
    , button "音" (f $ InpRad '音')
    , button "頁" (f $ InpRad '頁')
    , button "風" (f $ InpRad '風')
    , button "飛" (f $ InpRad '飛')
    , button "食" (f $ InpRad '食')
    , button "首" (f $ InpRad '首')
    , button "香" (f $ InpRad '香')
    , button "品" (f $ InpRad '品')

    , styleLabel $ label "10"
    , button "馬" (f $ InpRad '馬')
    , button "骨" (f $ InpRad '骨')
    , button "高" (f $ InpRad '高')
    , button "髟" (f $ InpRad '髟')
    , button "鬥" (f $ InpRad '鬥')
    , button "鬯" (f $ InpRad '鬯')
    , button "鬲" (f $ InpRad '鬲')
    , button "鬼" (f $ InpRad '鬼')
    , button "竜" (f $ InpRad '竜')
    , button "韋" (f $ InpRad '韋')

    , styleLabel $ label "11"
    , button "魚" (f $ InpRad '魚')
    , button "鳥" (f $ InpRad '鳥')
    , button "鹵" (f $ InpRad '鹵')
    , button "鹿" (f $ InpRad '鹿')
    , button "麻" (f $ InpRad '麻')
    , button "亀" (f $ InpRad '亀')
    , button "啇" (f $ InpRad '啇')
    , button "黄" (f $ InpRad '黄')
    , button "黒" (f $ InpRad '黒')

    , styleLabel $ label "12"
    , button "黍" (f $ InpRad '黍')
    , button "黹" (f $ InpRad '黹')
    , button "無" (f $ InpRad '無')
    , button "歯" (f $ InpRad '歯')

    , styleLabel $ label "13"
    , button "黽" (f $ InpRad '黽')
    , button "鼎" (f $ InpRad '鼎')
    , button "鼓" (f $ InpRad '鼓')
    , button "鼠" (f $ InpRad '鼠')

    , styleLabel $ label "14"
    , button "鼻" (f $ InpRad '鼻')
    , button "齊" (f $ InpRad '齊')

    , styleLabel $ label "17"
    , button "龠" (f $ InpRad '龠')

    ]
  where styleLabel =  (`styleBasic` [bgColor darkGray, border 1 black, radius 3, textCenter])
