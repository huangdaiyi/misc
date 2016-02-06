using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Newegg.Rebate
{
    public interface IRebate
    {
        decimal GetRebateAmt(RebateBaseInfo rebateBaseInfo);
    }
}
