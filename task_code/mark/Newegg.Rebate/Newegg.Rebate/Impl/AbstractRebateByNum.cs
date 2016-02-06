using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Newegg.Rebate
{
    public abstract class AbstractRebateByNum : IRebate
    {
        public decimal Amount { get; set; }
        public abstract decimal GetRebateAmt(RebateBaseInfo rebateBaseInfo);
    }
}
