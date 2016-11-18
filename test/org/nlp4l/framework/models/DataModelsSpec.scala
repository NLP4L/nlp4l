/*
 * Copyright 2016 org.NLP4L
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.nlp4l.framework.models

import org.specs2.mutable.Specification

class DataModelsSpec extends Specification {

  def compareHeads[A](list: Seq[A]): Boolean = list.length match {
    case 0 => true
    case 1 => true
    case _ => if(list.head.hashCode == list.tail.head.hashCode) false
    else compareHeads(list.tail)
  }

  "Cell.hashCode" should {
    val cells = Seq(
      Cell("name", "aa"), Cell("name", "bb"), Cell("name", "cc"), Cell("name", "dd"), Cell("name", "ee"),
      Cell("name", "ff"), Cell("name", "gg"), Cell("name", "hh"), Cell("name", "ii"), Cell("name", "jj"))
    "hashCode of a Cell must not equal to hashCode of different Cell" in {
      compareHeads(cells)
    }

    "hashCode of a Cell can be overridden by userDefinedHashCode" in {
      Cell("name", "aa").hashCode mustNotEqual Cell("name", "bb").hashCode
      Cell("name", "aa", _ => 0).hashCode mustEqual Cell("name", "bb", _=> 0).hashCode
      Cell("name", "aa", _ => 10).hashCode mustNotEqual Cell("name", "bb", _=> 100).hashCode
    }
  }

  "Record.hashCode" should {
    val records = Seq(
      Record(Seq(Cell("term", "選手"),Cell("score", 195300.77))),
      Record(Seq(Cell("term", "日本"),Cell("score", 148867.94))),
      Record(Seq(Cell("term", "日本代表"),Cell("score", 100434.33))),
      Record(Seq(Cell("term", "放送"),Cell("score", 85704.45))),
      Record(Seq(Cell("term", "試合"),Cell("score", 80241.945))),
      Record(Seq(Cell("term", "監督"),Cell("score", 51991.973))),
      Record(Seq(Cell("term", "一人"),Cell("score", 50364.668))),
      Record(Seq(Cell("term", "チーム"),Cell("score", 42169.906))),
      Record(Seq(Cell("term", "一戦"),Cell("score", 37174.258))),
      Record(Seq(Cell("term", "代表"),Cell("score", 36725.156))),
      Record(Seq(Cell("term", "番組"),Cell("score", 28967.604))),
      Record(Seq(Cell("term", "TBS"),Cell("score", 28751.39))),
      Record(Seq(Cell("term", "ファン"),Cell("score", 27837.07))),
      Record(Seq(Cell("term", "サッカー"),Cell("score", 27541.295))),
      Record(Seq(Cell("term", "野球"),Cell("score", 22878.709))),
      Record(Seq(Cell("term", "自分"),Cell("score", 22703.744))),
      Record(Seq(Cell("term", "深夜放送"),Cell("score", 21437.482))),
      Record(Seq(Cell("term", "二人"),Cell("score", 18731.07))),
      Record(Seq(Cell("term", "試合後"),Cell("score", 18593.898))),
      Record(Seq(Cell("term", "自身"),Cell("score", 17263.63))),
      Record(Seq(Cell("term", "放送分"),Cell("score", 16078.562))),
      Record(Seq(Cell("term", "出場"),Cell("score", 15132.277))),
      Record(Seq(Cell("term", "番組内"),Cell("score", 15111.707))),
      Record(Seq(Cell("term", "サッカー解説者"),Cell("score", 14323.614))),
      Record(Seq(Cell("term", "サッカー日本代表"),Cell("score", 14260.762))),
      Record(Seq(Cell("term", "優勝"),Cell("score", 14204.327))),
      Record(Seq(Cell("term", "日本時間"),Cell("score", 13801.132))),
      Record(Seq(Cell("term", "日本テレビ"),Cell("score", 13374.214))),
      Record(Seq(Cell("term", "一年"),Cell("score", 13267.402))),
      Record(Seq(Cell("term", "W杯"),Cell("score", 13040.237))),
      Record(Seq(Cell("term", "五輪"),Cell("score", 11790.585))),
      Record(Seq(Cell("term", "一回"),Cell("score", 11599.383))),
      Record(Seq(Cell("term", "一日"),Cell("score", 11491.427))),
      Record(Seq(Cell("term", "深夜"),Cell("score", 11342.081))),
      Record(Seq(Cell("term", "試合中"),Cell("score", 11216.443))),
      Record(Seq(Cell("term", "相手"),Cell("score", 11197.679))),
      Record(Seq(Cell("term", "ロンドン五輪"),Cell("score", 11148.927))),
      Record(Seq(Cell("term", "大会"),Cell("score", 10983.492))),
      Record(Seq(Cell("term", "プロ野球"),Cell("score", 10722.962))),
      Record(Seq(Cell("term", "コメント"),Cell("score", 10343.097))),
      Record(Seq(Cell("term", "現在"),Cell("score", 9992.7295))),
      Record(Seq(Cell("term", "野球解説者"),Cell("score", 9838.784))),
      Record(Seq(Cell("term", "世界"),Cell("score", 9467.928))),
      Record(Seq(Cell("term", "プロ"),Cell("score", 9259.884))),
      Record(Seq(Cell("term", "ネット上"),Cell("score", 9160.094))),
      Record(Seq(Cell("term", "なでしこジャパン"),Cell("score", 8739.683))),
      Record(Seq(Cell("term", "フジテレビ"),Cell("score", 8668.366))),
      Record(Seq(Cell("term", "プレー"),Cell("score", 8651.38))),
      Record(Seq(Cell("term", "試合前"),Cell("score", 8630.103))),
      Record(Seq(Cell("term", "サッカーファン"),Cell("score", 8300.27))),
      Record(Seq(Cell("term", "ゴール"),Cell("score", 8238.448))),
      Record(Seq(Cell("term", "一本"),Cell("score", 7816.4985))),
      Record(Seq(Cell("term", "今年"),Cell("score", 7724.123))),
      Record(Seq(Cell("term", "獲得"),Cell("score", 7460.335))),
      Record(Seq(Cell("term", "スポーツ番組"),Cell("score", 7386.986))),
      Record(Seq(Cell("term", "プロ野球解説者"),Cell("score", 7321.5205))),
      Record(Seq(Cell("term", "結果"),Cell("score", 7312.57))),
      Record(Seq(Cell("term", "プロ野球選手"),Cell("score", 7189.933))),
      Record(Seq(Cell("term", "サッカー女子日本代表"),Cell("score", 7155.7007))),
      Record(Seq(Cell("term", "女子"),Cell("score", 7128.2046))),
      Record(Seq(Cell("term", "週刊アサヒ芸能"),Cell("score", 7088.145))),
      Record(Seq(Cell("term", "サッカー選手"),Cell("score", 6976.6978))),
      Record(Seq(Cell("term", "出演"),Cell("score", 6756.938))),
      Record(Seq(Cell("term", "野球選手"),Cell("score", 6596.656))),
      Record(Seq(Cell("term", "リーグ"),Cell("score", 6552.1084))),
      Record(Seq(Cell("term", "最後"),Cell("score", 6539.589))),
      Record(Seq(Cell("term", "インタビュー"),Cell("score", 6358.4546))),
      Record(Seq(Cell("term", "関係者"),Cell("score", 6341.8213))),
      Record(Seq(Cell("term", "野村"),Cell("score", 6292.2886))),
      Record(Seq(Cell("term", "日本中"),Cell("score", 6254.2207))),
      Record(Seq(Cell("term", "解説"),Cell("score", 5930.0522))),
      Record(Seq(Cell("term", "ボール"),Cell("score", 5845.7))),
      Record(Seq(Cell("term", "テレビ"),Cell("score", 5749.4443))),
      Record(Seq(Cell("term", "報道"),Cell("score", 5714.914))),
      Record(Seq(Cell("term", "一発"),Cell("score", 5674.3916))),
      Record(Seq(Cell("term", "日本戦"),Cell("score", 5661.53))),
      Record(Seq(Cell("term", "発表"),Cell("score", 5536.19))),
      Record(Seq(Cell("term", "ブログ"),Cell("score", 5445.751))),
      Record(Seq(Cell("term", "以上"),Cell("score", 5350.005))),
      Record(Seq(Cell("term", "練習"),Cell("score", 5304.7783))),
      Record(Seq(Cell("term", "予選"),Cell("score", 5270.7793))),
      Record(Seq(Cell("term", "野村克也"),Cell("score", 5200.4653))),
      Record(Seq(Cell("term", "発売"),Cell("score", 5175.2134))),
      Record(Seq(Cell("term", "移籍"),Cell("score", 4949.08))),
      Record(Seq(Cell("term", "開催"),Cell("score", 4938.0537))),
      Record(Seq(Cell("term", "日本代表監督"),Cell("score", 4826.9585))),
      Record(Seq(Cell("term", "注目"),Cell("score", 4778.0835))),
      Record(Seq(Cell("term", "関係"),Cell("score", 4672.6816))),
      Record(Seq(Cell("term", "巨人"),Cell("score", 4641.379))),
      Record(Seq(Cell("term", "昨年"),Cell("score", 4483.5254))),
      Record(Seq(Cell("term", "本田圭佑"),Cell("score", 4381.0664))),
      Record(Seq(Cell("term", "勝利"),Cell("score", 4364.0234))),
      Record(Seq(Cell("term", "問題"),Cell("score", 4359.9927))),
      Record(Seq(Cell("term", "時間"),Cell("score", 4355.9546))),
      Record(Seq(Cell("term", "二日"),Cell("score", 4138.9707))),
      Record(Seq(Cell("term", "代表戦"),Cell("score", 4103.4604))),
      Record(Seq(Cell("term", "ロンドン五輪出場"),Cell("score", 4001.4502))),
      Record(Seq(Cell("term", "日本人選手"),Cell("score", 4000.7212))),
      Record(Seq(Cell("term", "今回"),Cell("score", 3930.5852))),
      Record(Seq(Cell("term", "本田"),Cell("score", 3866.496))),
      Record(Seq(Cell("term", "Jリーグ"),Cell("score", 3799.6055))),
      Record(Seq(Cell("term", "開幕"),Cell("score", 3780.0))),
      Record(Seq(Cell("term", "球団"),Cell("score", 3742.5552))),
      Record(Seq(Cell("term", "関連リンク"),Cell("score", 3737.1843))),
      Record(Seq(Cell("term", "ダルビッシュ有"),Cell("score", 3728.844))),
      Record(Seq(Cell("term", "選手達"),Cell("score", 3728.4832))),
      Record(Seq(Cell("term", "掲示板上"),Cell("score", 3677.5344))),
      Record(Seq(Cell("term", "テレビ朝日"),Cell("score", 3618.1233))),
      Record(Seq(Cell("term", "活躍"),Cell("score", 3586.053))),
      Record(Seq(Cell("term", "ダルビッシュ"),Cell("score", 3567.776))),
      Record(Seq(Cell("term", "コーナー"),Cell("score", 3513.1182))),
      Record(Seq(Cell("term", "五輪出場"),Cell("score", 3490.6548))),
      Record(Seq(Cell("term", "NEWS"),Cell("score", 3409.2607))),
      Record(Seq(Cell("term", "ライト版"),Cell("score", 3349.5254))),
      Record(Seq(Cell("term", "状態"),Cell("score", 3324.1084))),
      Record(Seq(Cell("term", "ツイッター上"),Cell("score", 3315.427))),
      Record(Seq(Cell("term", "一度"),Cell("score", 3312.918))),
      Record(Seq(Cell("term", "楽天"),Cell("score", 3308.9668))),
      Record(Seq(Cell("term", "気持ち"),Cell("score", 3267.632))),
      Record(Seq(Cell("term", "その後"),Cell("score", 3245.7556))),
      Record(Seq(Cell("term", "女子W杯"),Cell("score", 3243.5562))),
      Record(Seq(Cell("term", "なでしこ"),Cell("score", 3229.2607))),
      Record(Seq(Cell("term", "スポーツ選手"),Cell("score", 3222.894))),
      Record(Seq(Cell("term", "Sports"),Cell("score", 3217.3457))),
      Record(Seq(Cell("term", "話題"),Cell("score", 3136.6785))),
      Record(Seq(Cell("term", "女子サッカー"),Cell("score", 3133.0496))),
      Record(Seq(Cell("term", "オープン戦"),Cell("score", 3116.8618))),
      Record(Seq(Cell("term", "田中"),Cell("score", 3082.6482))),
      Record(Seq(Cell("term", "所属"),Cell("score", 3044.6675))),
      Record(Seq(Cell("term", "スポーツ"),Cell("score", 2959.8918))),
      Record(Seq(Cell("term", "開幕戦"),Cell("score", 2937.1902))),
      Record(Seq(Cell("term", "斎藤佑樹"),Cell("score", 2932.976))),
      Record(Seq(Cell("term", "一番"),Cell("score", 2915.3389))),
      Record(Seq(Cell("term", "日本選手権"),Cell("score", 2829.2852))),
      Record(Seq(Cell("term", "田中将大"),Cell("score", 2817.556))),
      Record(Seq(Cell("term", "代表選手"),Cell("score", 2812.8716))),
      Record(Seq(Cell("term", "日本代表選手"),Cell("score", 2811.5234))),
      Record(Seq(Cell("term", "サッカー界"),Cell("score", 2810.531))),
      Record(Seq(Cell("term", "シーズン"),Cell("score", 2802.811))),
      Record(Seq(Cell("term", "代表監督"),Cell("score", 2796.4863))),
      Record(Seq(Cell("term", "最終的"),Cell("score", 2773.3777))),
      Record(Seq(Cell("term", "ドイツ"),Cell("score", 2750.203))),
      Record(Seq(Cell("term", "北海道日本ハムファイターズ"),Cell("score", 2713.5928))),
      Record(Seq(Cell("term", "世界選手権"),Cell("score", 2705.5283))),
      Record(Seq(Cell("term", "一杯"),Cell("score", 2696.0342))),
      Record(Seq(Cell("term", "内容"),Cell("score", 2693.9934))),
      Record(Seq(Cell("term", "後半"),Cell("score", 2688.4553))),
      Record(Seq(Cell("term", "今月"),Cell("score", 2626.0024))),
      Record(Seq(Cell("term", "野球ファン"),Cell("score", 2587.2952))),
      Record(Seq(Cell("term", "決勝戦"),Cell("score", 2498.928))),
      Record(Seq(Cell("term", "長友"),Cell("score", 2494.0667))),
      Record(Seq(Cell("term", "一面"),Cell("score", 2492.322))),
      Record(Seq(Cell("term", "日本シリーズ"),Cell("score", 2459.9106))),
      Record(Seq(Cell("term", "人気"),Cell("score", 2454.7554))),
      Record(Seq(Cell("term", "PC版"),Cell("score", 2446.1482))),
      Record(Seq(Cell("term", "何人"),Cell("score", 2408.3223))),
      Record(Seq(Cell("term", "日本サッカー協会"),Cell("score", 2399.5002))),
      Record(Seq(Cell("term", "メンバー"),Cell("score", 2398.2463))),
      Record(Seq(Cell("term", "アスリート"),Cell("score", 2390.728))),
      Record(Seq(Cell("term", "批判"),Cell("score", 2379.694))),
      Record(Seq(Cell("term", "Going"),Cell("score", 2375.879))),
      Record(Seq(Cell("term", "ロンドン"),Cell("score", 2371.7505))),
      Record(Seq(Cell("term", "ネット"),Cell("score", 2363.5227))),
      Record(Seq(Cell("term", "五輪代表"),Cell("score", 2350.1277))),
      Record(Seq(Cell("term", "韓国戦"),Cell("score", 2333.2542))),
      Record(Seq(Cell("term", "公式戦"),Cell("score", 2325.3628))),
      Record(Seq(Cell("term", "解説者"),Cell("score", 2295.484))),
      Record(Seq(Cell("term", "星野監督"),Cell("score", 2247.6196))),
      Record(Seq(Cell("term", "日本サッカー"),Cell("score", 2244.1877))),
      Record(Seq(Cell("term", "番組中"),Cell("score", 2228.205))),
      Record(Seq(Cell("term", "深夜放送分"),Cell("score", 2224.8699))),
      Record(Seq(Cell("term", "野球界"),Cell("score", 2190.1917))),
      Record(Seq(Cell("term", "記録"),Cell("score", 2176.2354))),
      Record(Seq(Cell("term", "決勝"),Cell("score", 2170.8064))),
      Record(Seq(Cell("term", "News"),Cell("score", 2162.0))),
      Record(Seq(Cell("term", "写真"),Cell("score", 2161.5105))),
      Record(Seq(Cell("term", "期待"),Cell("score", 2159.1406))),
      Record(Seq(Cell("term", "ツイッター"),Cell("score", 2133.8865))),
      Record(Seq(Cell("term", "長友佑都"),Cell("score", 2091.0571))),
      Record(Seq(Cell("term", "日本代表戦"),Cell("score", 2069.8665))),
      Record(Seq(Cell("term", "最終戦"),Cell("score", 2032.4606))),
      Record(Seq(Cell("term", "彼女"),Cell("score", 2013.9241))),
      Record(Seq(Cell("term", "番組MC"),Cell("score", 1987.7031))),
      Record(Seq(Cell("term", "カメラ"),Cell("score", 1984.3488))),
      Record(Seq(Cell("term", "日本代表メンバー"),Cell("score", 1974.2808))),
      Record(Seq(Cell("term", "経験"),Cell("score", 1965.592))),
      Record(Seq(Cell("term", "プロ野球界"),Cell("score", 1945.1282))),
      Record(Seq(Cell("term", "報道ステーション"),Cell("score", 1937.5653))),
      Record(Seq(Cell("term", "好き"),Cell("score", 1929.8082))),
      Record(Seq(Cell("term", "引退"),Cell("score", 1926.5637))),
      Record(Seq(Cell("term", "ベスト"),Cell("score", 1918.9059))),
      Record(Seq(Cell("term", "女子日本代表"),Cell("score", 1913.1981))),
      Record(Seq(Cell("term", "代表チーム"),Cell("score", 1887.8099))),
      Record(Seq(Cell("term", "今日"),Cell("score", 1877.8086))),
      Record(Seq(Cell("term", "斎藤"),Cell("score", 1863.2358))),
      Record(Seq(Cell("term", "ロンドン五輪代表"),Cell("score", 1858.4434))),
      Record(Seq(Cell("term", "岡田監督"),Cell("score", 1853.7714))),
      Record(Seq(Cell("term", "可能性"),Cell("score", 1817.9286))),
      Record(Seq(Cell("term", "自分達"),Cell("score", 1803.7363))),
      Record(Seq(Cell("term", "同誌"),Cell("score", 1802.9451))),
      Record(Seq(Cell("term", "本田選手"),Cell("score", 1801.9078))),
      Record(Seq(Cell("term", "エース"),Cell("score", 1788.1276))),
      Record(Seq(Cell("term", "記者"),Cell("score", 1787.2885))),
      Record(Seq(Cell("term", "練習試合"),Cell("score", 1764.6514))),
      Record(Seq(Cell("term", "一歩"),Cell("score", 1734.5682))),
      Record(Seq(Cell("term", "メディア"),Cell("score", 1731.0251))),
      Record(Seq(Cell("term", "一年間"),Cell("score", 1724.5685))),
      Record(Seq(Cell("term", "理由"),Cell("score", 1709.0312))),
      Record(Seq(Cell("term", "時代"),Cell("score", 1706.7733))),
      Record(Seq(Cell("term", "澤穂希"),Cell("score", 1698.2334))),
      Record(Seq(Cell("term", "最初"),Cell("score", 1690.4674))),
      Record(Seq(Cell("term", "北京五輪"),Cell("score", 1681.2288))),
      Record(Seq(Cell("term", "デジタル"),Cell("score", 1680.0))),
      Record(Seq(Cell("term", "発言"),Cell("score", 1671.8552))),
      Record(Seq(Cell("term", "当時"),Cell("score", 1663.1151))),
      Record(Seq(Cell("term", "大会中"),Cell("score", 1648.9957))),
      Record(Seq(Cell("term", "巨人戦"),Cell("score", 1644.2094))),
      Record(Seq(Cell("term", "ジャパン"),Cell("score", 1638.8523))),
      Record(Seq(Cell("term", "数日"),Cell("score", 1632.1078))),
      Record(Seq(Cell("term", "ゲーム"),Cell("score", 1627.1952))),
      Record(Seq(Cell("term", "モバイル版"),Cell("score", 1626.6335))),
      Record(Seq(Cell("term", "アジア"),Cell("score", 1625.583))),
      Record(Seq(Cell("term", "球団関係者"),Cell("score", 1623.7137))),
      Record(Seq(Cell("term", "一点"),Cell("score", 1623.4846))),
      Record(Seq(Cell("term", "ザッケローニ監督"),Cell("score", 1618.3197))),
      Record(Seq(Cell("term", "何回"),Cell("score", 1608.5016))),
      Record(Seq(Cell("term", "チーム内"),Cell("score", 1598.6632))),
      Record(Seq(Cell("term", "更新"),Cell("score", 1583.2726))),
      Record(Seq(Cell("term", "一つ"),Cell("score", 1558.8457))),
      Record(Seq(Cell("term", "日韓戦"),Cell("score", 1526.6932))),
      Record(Seq(Cell("term", "日本サッカー界"),Cell("score", 1523.6971))),
      Record(Seq(Cell("term", "得点"),Cell("score", 1517.236))),
      Record(Seq(Cell("term", "香川"),Cell("score", 1517.1741))),
      Record(Seq(Cell("term", "アメリカ戦"),Cell("score", 1481.9059))),
      Record(Seq(Cell("term", "何度"),Cell("score", 1470.1013))),
      Record(Seq(Cell("term", "紹介"),Cell("score", 1470.0))),
      Record(Seq(Cell("term", "記事"),Cell("score", 1461.3296))),
      Record(Seq(Cell("term", "ブログ上"),Cell("score", 1459.7957))),
      Record(Seq(Cell("term", "日本選手"),Cell("score", 1455.341))),
      Record(Seq(Cell("term", "東北楽天ゴールデンイーグルス"),Cell("score", 1454.6908))),
      Record(Seq(Cell("term", "代表入り"),Cell("score", 1448.5029))),
      Record(Seq(Cell("term", "スポーツ紙"),Cell("score", 1448.2838))),
      Record(Seq(Cell("term", "連続"),Cell("score", 1447.6188))),
      Record(Seq(Cell("term", "韓国"),Cell("score", 1445.3235))),
      Record(Seq(Cell("term", "ZERO"),Cell("score", 1443.0))),
      Record(Seq(Cell("term", "競技"),Cell("score", 1440.8317))),
      Record(Seq(Cell("term", "何年"),Cell("score", 1424.3676))),
      Record(Seq(Cell("term", "決定戦"),Cell("score", 1423.7308))),
      Record(Seq(Cell("term", "対戦"),Cell("score", 1397.4076))),
      Record(Seq(Cell("term", "結婚"),Cell("score", 1388.6166))),
      Record(Seq(Cell("term", "投手"),Cell("score", 1375.6453))),
      Record(Seq(Cell("term", "原監督"),Cell("score", 1372.3314))),
      Record(Seq(Cell("term", "目標"),Cell("score", 1368.4955))),
      Record(Seq(Cell("term", "ツイート"),Cell("score", 1347.2194))),
      Record(Seq(Cell("term", "ダル"),Cell("score", 1330.8978))),
      Record(Seq(Cell("term", "アジアカップ"),Cell("score", 1326.2296))),
      Record(Seq(Cell("term", "コーチ"),Cell("score", 1324.3206))),
      Record(Seq(Cell("term", "質問"),Cell("score", 1323.6495))),
      Record(Seq(Cell("term", "様子"),Cell("score", 1319.7819))),
      Record(Seq(Cell("term", "W杯アジア"),Cell("score", 1318.6077))),
      Record(Seq(Cell("term", "評価"),Cell("score", 1313.6711))),
      Record(Seq(Cell("term", "五輪予選"),Cell("score", 1296.9718))),
      Record(Seq(Cell("term", "選手中"),Cell("score", 1296.2019))),
      Record(Seq(Cell("term", "アサヒ芸能"),Cell("score", 1295.9344))),
      Record(Seq(Cell("term", "意味"),Cell("score", 1294.4713))),
      Record(Seq(Cell("term", "今後"),Cell("score", 1292.7366))),
      Record(Seq(Cell("term", "アメリカ"),Cell("score", 1292.4473))),
      Record(Seq(Cell("term", "落合監督"),Cell("score", 1290.155))),
      Record(Seq(Cell("term", "契約"),Cell("score", 1279.4498))),
      Record(Seq(Cell("term", "W杯出場"),Cell("score", 1277.7772))),
      Record(Seq(Cell("term", "三人"),Cell("score", 1274.834))),
      Record(Seq(Cell("term", "女子サッカー界"),Cell("score", 1274.1223))),
      Record(Seq(Cell("term", "番組司会"),Cell("score", 1270.6456))),
      Record(Seq(Cell("term", "成長"),Cell("score", 1268.3335))),
      Record(Seq(Cell("term", "次戦"),Cell("score", 1267.8969))),
      Record(Seq(Cell("term", "レベル"),Cell("score", 1267.2706))),
      Record(Seq(Cell("term", "誕生日"),Cell("score", 1265.3956))),
      Record(Seq(Cell("term", "南アフリカW杯"),Cell("score", 1262.8958))),
      Record(Seq(Cell("term", "芸能界"),Cell("score", 1252.321))),
      Record(Seq(Cell("term", "ホーム"),Cell("score", 1246.9161))),
      Record(Seq(Cell("term", "日本勢"),Cell("score", 1241.7866))),
      Record(Seq(Cell("term", "一件"),Cell("score", 1239.2891))),
      Record(Seq(Cell("term", "選手会"),Cell("score", 1227.6753))),
      Record(Seq(Cell("term", "結果的"),Cell("score", 1225.2083))),
      Record(Seq(Cell("term", "金メダル"),Cell("score", 1224.1323))),
      Record(Seq(Cell("term", "リーグ優勝"),Cell("score", 1221.812))),
      Record(Seq(Cell("term", "大事"),Cell("score", 1221.6484))),
      Record(Seq(Cell("term", "説明"),Cell("score", 1219.846))),
      Record(Seq(Cell("term", "一番"),Cell("score", 1203.1417))),
      Record(Seq(Cell("term", "その他"),Cell("score", 1195.8328))),
      Record(Seq(Cell("term", "香川真司"),Cell("score", 1181.5005))),
      Record(Seq(Cell("term", "出演者"),Cell("score", 1179.174))),
      Record(Seq(Cell("term", "タレント"),Cell("score", 1178.4736))),
      Record(Seq(Cell("term", "テレビ番組"),Cell("score", 1169.4703))),
      Record(Seq(Cell("term", "感じ"),Cell("score", 1161.1719))),
      Record(Seq(Cell("term", "決定"),Cell("score", 1154.5874))),
      Record(Seq(Cell("term", "通り"),Cell("score", 1153.7764))),
      Record(Seq(Cell("term", "一個"),Cell("score", 1153.5101))),
      Record(Seq(Cell("term", "フォトギャラリー"),Cell("score", 1148.0233))),
      Record(Seq(Cell("term", "前半"),Cell("score", 1145.4148))),
      Record(Seq(Cell("term", "公式ブログ"),Cell("score", 1143.8538))),
      Record(Seq(Cell("term", "具体的"),Cell("score", 1137.1323))),
      Record(Seq(Cell("term", "ソフトバンク"),Cell("score", 1130.3274))),
      Record(Seq(Cell("term", "松井"),Cell("score", 1130.1664))),
      Record(Seq(Cell("term", "精神的"),Cell("score", 1124.7838))),
      Record(Seq(Cell("term", "会見"),Cell("score", 1121.7843))),
      Record(Seq(Cell("term", "最終予選"),Cell("score", 1120.3832))),
      Record(Seq(Cell("term", "出場選手"),Cell("score", 1119.0991))),
      Record(Seq(Cell("term", "相手選手"),Cell("score", 1118.4438))),
      Record(Seq(Cell("term", "情報"),Cell("score", 1117.336))),
      Record(Seq(Cell("term", "女性アスリート"),Cell("score", 1094.3627))),
      Record(Seq(Cell("term", "個人的"),Cell("score", 1092.2646))),
      Record(Seq(Cell("term", "なでしこリーグ"),Cell("score", 1088.5193))),
      Record(Seq(Cell("term", "メジャー"),Cell("score", 1085.8802))),
      Record(Seq(Cell("term", "東京"),Cell("score", 1083.882))),
      Record(Seq(Cell("term", "開催中"),Cell("score", 1079.3448))),
      Record(Seq(Cell("term", "注目選手"),Cell("score", 1076.146))),
      Record(Seq(Cell("term", "プロサッカー選手"),Cell("score", 1071.2183))),
      Record(Seq(Cell("term", "言葉"),Cell("score", 1068.4634))),
      Record(Seq(Cell("term", "ピッチャー"),Cell("score", 1062.4575))),
      Record(Seq(Cell("term", "一週間"),Cell("score", 1059.0358))),
      Record(Seq(Cell("term", "オリンピック"),Cell("score", 1045.6099))),
      Record(Seq(Cell("term", "クラブ"),Cell("score", 1043.3792))),
      Record(Seq(Cell("term", "日本人"),Cell("score", 1031.8295))),
      Record(Seq(Cell("term", "応援"),Cell("score", 1028.3657))),
      Record(Seq(Cell("term", "長谷部"),Cell("score", 1028.1323))),
      Record(Seq(Cell("term", "自分自身"),Cell("score", 1026.3269))),
      Record(Seq(Cell("term", "ドイツ戦"),Cell("score", 1021.4441))),
      Record(Seq(Cell("term", "ニュース"),Cell("score", 1021.37115))),
      Record(Seq(Cell("term", "引退後"),Cell("score", 1016.3294))),
      Record(Seq(Cell("term", "女性"),Cell("score", 1004.98755))),
      Record(Seq(Cell("term", "存在"),Cell("score", 999.968))),
      Record(Seq(Cell("term", "プロ野球チーム"),Cell("score", 999.9159))),
      Record(Seq(Cell("term", "外国人選手"),Cell("score", 993.50726))),
      Record(Seq(Cell("term", "美女アスリート"),Cell("score", 988.7709))),
      Record(Seq(Cell("term", "情報番組"),Cell("score", 987.03796))),
      Record(Seq(Cell("term", "女子W杯ドイツ"),Cell("score", 985.45776))),
      Record(Seq(Cell("term", "全て"),Cell("score", 975.9918))),
      Record(Seq(Cell("term", "舞台"),Cell("score", 974.0164))),
      Record(Seq(Cell("term", "笑顔"),Cell("score", 966.6437))),
      Record(Seq(Cell("term", "記者会見"),Cell("score", 965.4295))),
      Record(Seq(Cell("term", "野球報道"),Cell("score", 959.4344))),
      Record(Seq(Cell("term", "一球"),Cell("score", 958.4992))),
      Record(Seq(Cell("term", "開幕日"),Cell("score", 956.5941))),
      Record(Seq(Cell("term", "サッカーW杯アジア最終予選"),Cell("score", 953.6853))),
      Record(Seq(Cell("term", "ロンドン五輪予選"),Cell("score", 949.09076))),
      Record(Seq(Cell("term", "試合内容"),Cell("score", 943.5468))),
      Record(Seq(Cell("term", "W杯優勝"),Cell("score", 934.148))),
      Record(Seq(Cell("term", "一位"),Cell("score", 927.68933))),
      Record(Seq(Cell("term", "プロゴルファー"),Cell("score", 912.82623))),
      Record(Seq(Cell("term", "視聴者"),Cell("score", 912.1704))),
      Record(Seq(Cell("term", "プロ野球公式戦"),Cell("score", 911.6793))),
      Record(Seq(Cell("term", "ネット掲示板上"),Cell("score", 908.30585))),
      Record(Seq(Cell("term", "高校時代"),Cell("score", 905.6523))),
      Record(Seq(Cell("term", "野村監督"),Cell("score", 902.18))),
      Record(Seq(Cell("term", "代表メンバー"),Cell("score", 900.9773))),
      Record(Seq(Cell("term", "掲載"),Cell("score", 899.82))),
      Record(Seq(Cell("term", "必要"),Cell("score", 895.4887))),
      Record(Seq(Cell("term", "状況"),Cell("score", 893.6442))),
      Record(Seq(Cell("term", "絶対"),Cell("score", 890.7862))),
      Record(Seq(Cell("term", "一緒"),Cell("score", 886.8483))),
      Record(Seq(Cell("term", "世界戦"),Cell("score", 882.6989))),
      Record(Seq(Cell("term", "登場"),Cell("score", 875.3468))),
      Record(Seq(Cell("term", "横浜"),Cell("score", 874.29974))),
      Record(Seq(Cell("term", "佐々木監督"),Cell("score", 873.5013))),
      Record(Seq(Cell("term", "大会後"),Cell("score", 863.24176))),
      Record(Seq(Cell("term", "韓国代表"),Cell("score", 860.5287))),
      Record(Seq(Cell("term", "注目度"),Cell("score", 856.0245))),
      Record(Seq(Cell("term", "W杯アジア最終予選"),Cell("score", 847.44836))),
      Record(Seq(Cell("term", "女子選手"),Cell("score", 846.5674))),
      Record(Seq(Cell("term", "ゴルフ"),Cell("score", 842.4963))),
      Record(Seq(Cell("term", "人気選手"),Cell("score", 839.73364))),
      Record(Seq(Cell("term", "開幕前"),Cell("score", 832.1679))),
      Record(Seq(Cell("term", "過去"),Cell("score", 823.02856))),
      Record(Seq(Cell("term", "取材"),Cell("score", 818.8284))),
      Record(Seq(Cell("term", "ゲスト"),Cell("score", 816.7221))),
      Record(Seq(Cell("term", "日本代表MF本田圭佑"),Cell("score", 811.1646))),
      Record(Seq(Cell("term", "フォト"),Cell("score", 807.5828))),
      Record(Seq(Cell("term", "サッカー番組"),Cell("score", 799.54175))),
      Record(Seq(Cell("term", "ゲスト解説"),Cell("score", 798.9834))),
      Record(Seq(Cell("term", "ゴール後"),Cell("score", 798.5429))),
      Record(Seq(Cell("term", "日本プロレス"),Cell("score", 796.1001))),
      Record(Seq(Cell("term", "横浜F"),Cell("score", 795.238))),
      Record(Seq(Cell("term", "帰国後"),Cell("score", 794.968))),
      Record(Seq(Cell("term", "人気コーナー"),Cell("score", 794.306))),
      Record(Seq(Cell("term", "ダメ"),Cell("score", 792.30676))),
      Record(Seq(Cell("term", "北朝鮮代表"),Cell("score", 789.49115))),
      Record(Seq(Cell("term", "アメリカ代表"),Cell("score", 789.45026))),
      Record(Seq(Cell("term", "インターネット上"),Cell("score", 786.8436))),
      Record(Seq(Cell("term", "浅田真央"),Cell("score", 786.0774))),
      Record(Seq(Cell("term", "ドイツ代表"),Cell("score", 783.57623))),
      Record(Seq(Cell("term", "ゴール前"),Cell("score", 782.44684))),
      Record(Seq(Cell("term", "部屋"),Cell("score", 782.30426))),
      Record(Seq(Cell("term", "日本チーム"),Cell("score", 781.3815))),
      Record(Seq(Cell("term", "高校"),Cell("score", 774.4553))),
      Record(Seq(Cell("term", "数人"),Cell("score", 767.3939))),
      Record(Seq(Cell("term", "エピソード"),Cell("score", 763.37146))),
      Record(Seq(Cell("term", "シーズン中"),Cell("score", 763.25574))),
      Record(Seq(Cell("term", "日本代表DF長友佑都"),Cell("score", 762.6377))),
      Record(Seq(Cell("term", "日韓W杯"),Cell("score", 758.2106))),
      Record(Seq(Cell("term", "予定"),Cell("score", 756.5818))),
      Record(Seq(Cell("term", "五分"),Cell("score", 755.5554))),
      Record(Seq(Cell("term", "電話"),Cell("score", 754.0))),
      Record(Seq(Cell("term", "魔裟斗"),Cell("score", 743.559))),
      Record(Seq(Cell("term", "北京五輪時"),Cell("score", 732.5519))),
      Record(Seq(Cell("term", "リーグ戦"),Cell("score", 730.7985))),
      Record(Seq(Cell("term", "プロ野球開幕"),Cell("score", 728.32117))),
      Record(Seq(Cell("term", "ドイツW杯"),Cell("score", 726.1049))),
      Record(Seq(Cell("term", "本人"),Cell("score", 724.3659))),
      Record(Seq(Cell("term", "サポーター"),Cell("score", 724.15466))),
      Record(Seq(Cell("term", "オフ"),Cell("score", 722.807))),
      Record(Seq(Cell("term", "二年"),Cell("score", 719.3651))),
      Record(Seq(Cell("term", "アジア最終予選"),Cell("score", 716.8489))),
      Record(Seq(Cell("term", "長友選手"),Cell("score", 716.17706))),
      Record(Seq(Cell("term", "プレミアリーグ"),Cell("score", 713.11053))),
      Record(Seq(Cell("term", "なでしこ達"),Cell("score", 712.2288))),
      Record(Seq(Cell("term", "ネット掲示板"),Cell("score", 711.66876))),
      Record(Seq(Cell("term", "シュート"),Cell("score", 699.92))),
      Record(Seq(Cell("term", "掲示板"),Cell("score", 699.92))),
      Record(Seq(Cell("term", "マー君"),Cell("score", 698.85706))),
      Record(Seq(Cell("term", "ノムさん"),Cell("score", 698.36017))),
      Record(Seq(Cell("term", "挑戦"),Cell("score", 697.137))),
      Record(Seq(Cell("term", "大会連続"),Cell("score", 695.1174))),
      Record(Seq(Cell("term", "試合以上"),Cell("score", 694.39496))),
      Record(Seq(Cell("term", "長谷部誠"),Cell("score", 691.6641))),
      Record(Seq(Cell("term", "男子"),Cell("score", 689.3011))),
      Record(Seq(Cell("term", "正直"),Cell("score", 688.7089))),
      Record(Seq(Cell("term", "代表批判"),Cell("score", 686.3892))),
      Record(Seq(Cell("term", "北朝鮮戦"),Cell("score", 686.1031))),
      Record(Seq(Cell("term", "なでしこジャパンメンバー"),Cell("score", 685.118))),
      Record(Seq(Cell("term", "何日"),Cell("score", 682.94196))),
      Record(Seq(Cell("term", "勝ち点"),Cell("score", 682.84845))),
      Record(Seq(Cell("term", "カメルーン戦"),Cell("score", 680.9063))),
      Record(Seq(Cell("term", "五輪後"),Cell("score", 680.3124))),
      Record(Seq(Cell("term", "海外"),Cell("score", 678.79895))),
      Record(Seq(Cell("term", "公式サイト上"),Cell("score", 678.72943))),
      Record(Seq(Cell("term", "スポーツ紙デスク"),Cell("score", 678.3332))),
      Record(Seq(Cell("term", "最近"),Cell("score", 674.4331))),
      Record(Seq(Cell("term", "自ら"),Cell("score", 666.0))),
      Record(Seq(Cell("term", "成績"),Cell("score", 665.1075))),
      Record(Seq(Cell("term", "阪神"),Cell("score", 664.7285))),
      Record(Seq(Cell("term", "最終日"),Cell("score", 661.9387))),
      Record(Seq(Cell("term", "トップ"),Cell("score", 659.6363))),
      Record(Seq(Cell("term", "日本野球機構"),Cell("score", 657.5838))),
      Record(Seq(Cell("term", "試合連続"),Cell("score", 656.6913))),
      Record(Seq(Cell("term", "失点"),Cell("score", 654.9565))),
      Record(Seq(Cell("term", "報道番組"),Cell("score", 653.5672))),
      Record(Seq(Cell("term", "同日"),Cell("score", 651.0))),
      Record(Seq(Cell("term", "宮市"),Cell("score", 647.7198))),
      Record(Seq(Cell("term", "女子代表チーム"),Cell("score", 647.1056))),
      Record(Seq(Cell("term", "五輪最終予選"),Cell("score", 646.83496))),
      Record(Seq(Cell("term", "人生"),Cell("score", 643.28845))),
      Record(Seq(Cell("term", "W杯最終予選"),Cell("score", 640.396))),
      Record(Seq(Cell("term", "中継"),Cell("score", 636.18866))),
      Record(Seq(Cell("term", "イタリア"),Cell("score", 636.14856))),
      Record(Seq(Cell("term", "大会前"),Cell("score", 634.38116))),
      Record(Seq(Cell("term", "東北楽天ゴールデンイーグルス監督"),Cell("score", 633.0456))),
      Record(Seq(Cell("term", "阪神戦"),Cell("score", 631.8537))),
      Record(Seq(Cell("term", "ロンドン五輪アジア最終予選"),Cell("score", 625.5232))),
      Record(Seq(Cell("term", "チーム力"),Cell("score", 625.2632))),
      Record(Seq(Cell("term", "現役時代"),Cell("score", 624.90015))),
      Record(Seq(Cell("term", "僕自身"),Cell("score", 623.1017))),
      Record(Seq(Cell("term", "中日"),Cell("score", 622.2765))),
      Record(Seq(Cell("term", "存在感"),Cell("score", 621.0076))),
      Record(Seq(Cell("term", "勝負"),Cell("score", 620.4128))),
      Record(Seq(Cell("term", "日本代表キャプテン"),Cell("score", 615.59064))),
      Record(Seq(Cell("term", "野球部"),Cell("score", 614.7345))),
      Record(Seq(Cell("term", "美人解説者"),Cell("score", 614.7136))),
      Record(Seq(Cell("term", "ミス"),Cell("score", 614.4266))),
      Record(Seq(Cell("term", "メダル"),Cell("score", 612.19604))),
      Record(Seq(Cell("term", "ヤクルト戦"),Cell("score", 611.56744))),
      Record(Seq(Cell("term", "司会"),Cell("score", 608.6378))),
      Record(Seq(Cell("term", "マスコミ"),Cell("score", 607.35))),
      Record(Seq(Cell("term", "技術的"),Cell("score", 605.12787))),
      Record(Seq(Cell("term", "選手たち"),Cell("score", 604.1222))),
      Record(Seq(Cell("term", "アジア杯"),Cell("score", 603.99866))),
      Record(Seq(Cell("term", "番組カメラ"),Cell("score", 600.79395))),
      Record(Seq(Cell("term", "相手チーム"),Cell("score", 600.4994))),
      Record(Seq(Cell("term", "彼女達"),Cell("score", 600.12726))),
      Record(Seq(Cell("term", "練習中"),Cell("score", 591.54614))),
      Record(Seq(Cell("term", "人気者"),Cell("score", 590.5836))),
      Record(Seq(Cell("term", "日本代表時"),Cell("score", 589.59094))),
      Record(Seq(Cell("term", "日本記録"),Cell("score", 586.3769))),
      Record(Seq(Cell("term", "W杯予選"),Cell("score", 580.8876))),
      Record(Seq(Cell("term", "日本選手団"),Cell("score", 579.4277))),
      Record(Seq(Cell("term", "後半戦"),Cell("score", 576.07916))),
      Record(Seq(Cell("term", "タイ戦"),Cell("score", 573.3114))),
      Record(Seq(Cell("term", "一部"),Cell("score", 573.2905))),
      Record(Seq(Cell("term", "日本女子サッカー界"),Cell("score", 572.7185))),
      Record(Seq(Cell("term", "W杯時"),Cell("score", 571.1817))),
      Record(Seq(Cell("term", "高校野球"),Cell("score", 570.5121))),
      Record(Seq(Cell("term", "生活"),Cell("score", 569.20996))),
      Record(Seq(Cell("term", "サッカーW杯アジア"),Cell("score", 567.9613))),
      Record(Seq(Cell("term", "スタジアム"),Cell("score", 565.57227))),
      Record(Seq(Cell("term", "トップ選手"),Cell("score", 565.47266))),
      Record(Seq(Cell("term", "部分"),Cell("score", 564.8504))),
      Record(Seq(Cell("term", "プロレス"),Cell("score", 564.09216))),
      Record(Seq(Cell("term", "落合"),Cell("score", 560.0571))),
      Record(Seq(Cell("term", "ホームラン"),Cell("score", 558.48364))),
      Record(Seq(Cell("term", "パス"),Cell("score", 557.7096)))
    )
    "hashCode of a Record must not equal to hashCode of different Record" in {
      compareHeads(records)
    }

    "hashCode of a Record must always return same value" in {
      val ha1 = records.map(r => r.hashCode)
      val ha2 = records.map(r => r.hashCode)
      ha1.sameElements(ha2)
    }

    val record = Record(Seq(Cell("n1", "v1"), Cell("n2", "v2"), Cell("n3", 3)))

    "Record" should {
      "generate a Map so that one can get a certan Cell by name from the list of Cells" in {
        record.cellMap.get("nX") must_== None
        record.cellMap.get("n1") must_!= None
        record.cellMap.get("n1").get.value must_== "v1"
        record.cellMap.get("n2") must_!= None
        record.cellMap.get("n2").get.value must_== "v2"
        record.cellMap.get("n3") must_!= None
        record.cellMap.get("n3").get.value must_== 3
      }

      /*
       * Record.merge is never called if those cells don't have same key at the moment
       */
      "merge two cells" in {
        val r = Record(Seq(Cell("n1", "v1"), Cell("n2", "myval")))
        val cell = record.merge("n1", "/", r).cellMap.get("n2")
        cell must_!= None
        cell.get.value must_== "v2/myval"
      }
    }

    "Record.cellValue" should {
      "return None when there is no such name" in {
        record.cellValue("nX") must_== None
      }

      "return value" in {
        record.cellValue("n1") must_!= None
        record.cellValue("n1").get must_== "v1"
        record.cellValue("n2").get must_== "v2"
        record.cellValue("n2") must_!= None
        record.cellValue("n3").get must_== 3
        record.cellValue("n3") must_!= None
      }
    }

    "Record.canMerge" should {
      "work correctly even when a cell is empty" in {
        record.canMerge("name", Record(Seq())) must_== false
        Record(Seq()).canMerge("name", record) must_== false
      }

      "work correctly when a key that is never appeared" in {
        record.canMerge("nokey", record) must_== false
      }

      "return false when there is no same names between two cells" in {
        val r = Record(Seq(Cell("na1", "v1"), Cell("na2", "v2")))
        record.canMerge("n1", r) must_== false
      }

      "return true when two cells has a same cell name" in {
        val r = Record(Seq(Cell("na1", "v1"), Cell("n2", "v2")))
        record.canMerge("n2", r) must_== true
      }
    }

    "Record.mkCsvRecord" should {
      "make a CSV record as appropriate" in {
        val r = Record(Seq(Cell("na1", "va11"), Cell("na2", "va12"), Cell("na3", 100)))
        r.mkCsvRecord(",") must_== s""""va11","va12","100""""
      }
    }
  }

  "Dictionary.cellList" should {

    val dic = Dictionary(Seq(
      Record(Seq(Cell("na1", "va11"), Cell("na2", "va12"), Cell("na3", 100))),
      Record(Seq(Cell("na1", "va21"), Cell("na2", "va22"), Cell("na3", 200))),
      Record(Seq(Cell("na1", "va31"), Cell("na2", "va32"), Cell("na3", 300)))
    ))

    "return a list of the Cell strings" in {
      dic.cellList[String]("na2", (a) => a.toString) must_== Seq("va12", "va22", "va32")
    }

    "return a list of the Cell integers" in {
      dic.cellList[Int]("na3", (a) => a.toString.toInt) must_== Seq(100, 200, 300)
    }

    "throw an Exception when the specified cell name is not found" in {
      dic.cellList[String]("XXXXX", (a) => a.toString) must throwA[IllegalArgumentException]
    }
  }
}
