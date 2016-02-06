using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Frank.Task1.Model;

namespace Frank.Task1.Business
{
    public interface ICalculatorProfile
    {
        /// <summary>
        /// 按照数量计算利润
        /// </summary>
        /// <param name="num">数量</param>
        /// <param name="rules">规则</param>
        /// <returns>利润</returns>
        double GetProfileByCount(int num, CalculatorRulesModel rules);

        /// <summary>
        /// 按照金额计算利润
        /// </summary>
        /// <param name="money">金额</param>
        /// <param name="rules">返利百分比</param>
        /// <returns>返利</returns>
        double GetProfileByMoney(double money, CalculatorRulesModel rules);
    }
}
