using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Frank.Task1.Model;

namespace Frank.Task1.Business
{
    public class CalculatorProfile : ICalculatorProfile
    {
        /// <summary>
        /// 按照数量计算利润
        /// </summary>
        /// <param name="num">数量</param>
        /// <param name="rules">规则</param>
        /// <returns>利润</returns>
        public double GetProfileByCount(int num, CalculatorRulesModel rules)
        {
            return num < rules.Num ? 0 : num * rules.SignalProfile;
        }

        /// <summary>
        /// 按照金额计算利润
        /// </summary>
        /// <param name="money">金额</param>
        /// <param name="rules">返利百分比</param>
        /// <returns>返利</returns>
        public double GetProfileByMoney(double money, CalculatorRulesModel rules)
        {
            return money <= rules.AllMoney ? 0 : money * rules.ProfilePercent;
        }
    }
}
