using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Frank.Task1.Business;
using Frank.Task1.Model;
using Frank.Task1.XmlDataAccess;

namespace Frank.Task1.UI
{
    public class Program
    {
        public static void Main(string[] args)
        {
            MainEnum();
        }

        /// <summary>
        /// 操作界面
        /// </summary>
        private static void MainEnum()
        {
            ICalculatorProfile calculatorProfile = new CalculatorProfile();
            List<CalculatorRulesModel> lstRulesModel = XmlReader.GetRules();
            CalculatorType ruleType;
            string flag = "Y";
            while (flag == "Y")
            {
                ShowRules(lstRulesModel);
                ruleType = GetCalculatorType();
                CalculatorRulesModel ruleModel = lstRulesModel.Where(x => x.RuleType==ruleType).First(); //找到选中的规则
                if (ruleModel.RuleType == CalculatorType.One || ruleModel.RuleType == CalculatorType.Two)
                {
                    Console.Write("请输入商品数量:");
                    int num = 0;
                    string numStr = Console.ReadLine();
                    int.TryParse(numStr, out num);
                    Console.WriteLine("总利润为：{0}", calculatorProfile.GetProfileByCount(num, ruleModel));
                }
                else if (ruleModel.RuleType == CalculatorType.One || ruleModel.RuleType == CalculatorType.Four)
                {
                    Console.Write("请输入总金额:");
                    double money = 0;
                    string moneyStr = Console.ReadLine();
                    double.TryParse(moneyStr, out money);
                    Console.WriteLine("总利润为:{0}", calculatorProfile.GetProfileByMoney(money, ruleModel));
                }

                Console.Write("是否继续计算(N/Y)");
                flag = Console.ReadLine();
            }
        }
        
        /// <summary>
        /// 显示规则
        /// </summary>
        /// <param name="lstRulesModel">规则列表</param>
        private static void ShowRules(List<CalculatorRulesModel> lstRulesModel)
        {
            Console.WriteLine("-------------------------------------");

            foreach (CalculatorRulesModel model in lstRulesModel)
            {
                Console.WriteLine(string.Format("{0}:{1}", model.RuleType, model.Desc));
            }
            Console.WriteLine("-------------------------------------");
        }

        /// <summary>
        /// 校验选择的计算方式格式是否正确
        /// </summary>
        /// <returns></returns>
        private static CalculatorType GetCalculatorType()
        {
            Console.Write("请输入你的计算类型(One,Two,Three,Four):");
            CalculatorType type;    
            while (true)
            {
                try
                {
                    string typeStr = Console.ReadLine();
                    type = (CalculatorType)Enum.Parse(typeof(CalculatorType), typeStr);
                    return type;
                }
                catch (Exception)
                {
                    Console.Write("不合法数据，请重新选择计算方式:");
                }
            }
        }
    }
}
