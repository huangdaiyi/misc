using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Frank.Task1.Model
{
    public class CalculatorRulesModel
    {
        #region field

        /// <summary>
        /// 规则描述
        /// </summary>
        private string desc = string.Empty;

        /// <summary>
        /// 计算规则
        /// </summary>
        private CalculatorType ruleType = CalculatorType.One;

        /// <summary>
        /// 商品数量
        /// </summary>
        private int num = 0;

        /// <summary>
        /// 单个商品的利润
        /// </summary>
        private double signalProfile = 0;

        /// <summary>
        /// 商品总金额
        /// </summary>
        private double allMoney = 0;

        /// <summary>
        /// 返利百分比
        /// </summary>
        private double profilePercent = 0;
        #endregion

        public CalculatorRulesModel() { }

        #region　属性

        public string Desc
        {
            get { return this.desc; }
            set { this.desc = value; }
        }

        public CalculatorType RuleType
        {
            get { return this.ruleType; }
            set { this.ruleType = value; }
        }

        public int Num
        {
            get { return this.num; }
            set { this.num = value; }
        }

        public double SignalProfile
        {
            get { return this.signalProfile; }
            set { this.signalProfile = value; }
        }

        public double AllMoney
        {
            get { return this.allMoney; }
            set { this.allMoney = value; }
        }

        public double ProfilePercent
        {
            get { return this.profilePercent; }
            set { this.profilePercent = value; }
        }
        #endregion
    }
}
