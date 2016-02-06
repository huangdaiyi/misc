using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Frank.Task1.Model;
using System.Xml;

namespace Frank.Task1.XmlDataAccess
{
    public class XmlReader
    {
        /// <summary>
        /// 读取计算规则信息
        /// </summary>
        /// <returns>计算规则</returns>
        public static List<CalculatorRulesModel> GetRules()
        {
            string xmlPath = "rulesSheet.XML";
            XmlDocument document = new XmlDocument();
            List<CalculatorRulesModel> lstRulesModel = new List<CalculatorRulesModel>();
            try
            {
                document.Load(xmlPath);
                XmlElement rootElem = document.DocumentElement;
                XmlNodeList rulesNodes = rootElem.GetElementsByTagName("rule");
                CalculatorRulesModel model = null;
                foreach (XmlNode node in rulesNodes)
                {
                    string type = node.Attributes["type"].Value;
                    CalculatorType currentType = (CalculatorType)Enum.Parse(typeof(CalculatorType), type, true);
                    switch (currentType)
                    {
                        case CalculatorType.One:
                            model = new CalculatorRulesModel()
                            {
                                Num = int.Parse(node.Attributes["minNum"].Value),
                                RuleType = currentType,
                                SignalProfile = double.Parse(node.Attributes["signalProfile"].Value),
                                Desc = node.Attributes["Desc"].Value
                            };
                            break;
                        case CalculatorType.Two:
                            model = new CalculatorRulesModel()
                            {
                                Num = int.Parse(node.Attributes["minNum"].Value),
                                RuleType = currentType,
                                SignalProfile = double.Parse(node.Attributes["signalProfile"].Value),
                                Desc = node.Attributes["Desc"].Value
                            };
                            break;
                        case CalculatorType.Three:
                            model = new CalculatorRulesModel()
                            {
                                Num = int.Parse(node.Attributes["minMoney"].Value),
                                RuleType = currentType,
                                SignalProfile = double.Parse(node.Attributes["profilePercent"].Value),
                                Desc = node.Attributes["Desc"].Value
                            };
                            break;
                        case CalculatorType.Four:
                            model = new CalculatorRulesModel()
                            {
                                Num = int.Parse(node.Attributes["minMoney"].Value),
                                RuleType = currentType,
                                SignalProfile = double.Parse(node.Attributes["profilePercent"].Value),
                                Desc = node.Attributes["Desc"].Value
                            };
                            break;
                    }
                    lstRulesModel.Add(model);
                }

                return lstRulesModel;
            }
            catch (Exception ex)
            {
                throw new Exception("配置文件丢失");
            }
        }
    }
}
