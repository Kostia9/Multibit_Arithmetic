#include <iostream>
#include <cassert>
#include <chrono>
#include <random>
#include <string>
#include <intrin.h>


//Count Leading Zeros
int custom_clz(uint32_t x) {
    if (x == 0) return 32; // якщо чиCло 0, повертаємо 32 (вCі біти нульові)

    int leading_zeros = 0;
    for (int i = 31; i >= 0; --i) {
        if ((x & (static_cast<uint32_t>(1) << i)) == 0) {
            leading_zeros++;
        }
        else {
            break;
        }
    }
    return leading_zeros;
}


class BigNumber {
private:
    static const int n = 64; // 2048 bit
    uint32_t blocks[n];
public:
    //Конструктор без параметрів
    BigNumber() {
        memset(blocks, 0, sizeof(blocks));
    }

    //Конструктор з параметром
    BigNumber(uint32_t value) {
        memset(blocks, 0, sizeof(blocks));
        blocks[0] = value;
    }

    //Конструктор, який приймає масив у системі числення 2^32
    BigNumber(uint32_t a[n]) {
        for (int i = 0; i < n; ++i) {
            blocks[i] = a[i];
        }
    }

    // Конструктор, який приймає рядок у системі числення
    BigNumber(const std::string& str, int base = 10) {
        memset(blocks, 0, sizeof(blocks));

        if (base != 2 && base != 10 && base != 16) {
            std::cerr << "Unsupported base\n";
            return;
        }

        BigNumber multiplier(1);
        for (int i = str.length() - 1; i >= 0; --i) {
            char c = str[i];
            int digit = (c >= 'A') ? (c - 'A' + 10) : (c - '0');
            if (digit >= base) {
                std::cerr << "Invalid character for base\n";
                return;
            }
            *this = *this + BigNumber(digit) * multiplier;
            multiplier = multiplier * BigNumber(base);
        }
    }

    BigNumber& operator=(const BigNumber& other) {
        if (this != &other) {  // перевірка на приCвоювання Cамому Cобі
            for (int i = 0; i < n; ++i) {
                blocks[i] = other.blocks[i];
            }
        }
        return *this;
    }

    BigNumber operator+(const BigNumber& other) const {

        BigNumber result;
        uint64_t carry = 0;
        for (int i = 0; i < n; ++i) {
            uint64_t temp = static_cast<uint64_t>(blocks[i]) + other.blocks[i] + carry;
            result.blocks[i] = temp & UINT32_MAX;
            carry = temp >> 32;
        }
        return result;
    }

    BigNumber operator-(const BigNumber& other) const {
        BigNumber result;
        int64_t borrow = 0;  // ВикориCтовуємо int64_t тут для коректної роботи з негативними temp

        for (int i = 0; i < n; ++i) {
            int64_t temp = static_cast<int64_t>(blocks[i]) - other.blocks[i] - borrow;
            if (temp >= 0) {
                result.blocks[i] = static_cast<uint32_t>(temp);
                borrow = 0;
            }
            else {
                result.blocks[i] = static_cast<uint32_t>(temp + UINT32_MAX) + 1;
                borrow = 1;
            }
        }

        if (borrow) {
            //std::cerr << "\nError: Negative value in subtraction.\n";
            return BigNumber(); // повертаємо 0
        }

        return result;
    }

    static int LongCmp(const BigNumber& A, const BigNumber& B) {
        for (int i = n - 1; i >= 0; --i) {
            if (A.blocks[i] > B.blocks[i]) return 1;
            if (A.blocks[i] < B.blocks[i]) return -1;
        }
        return 0;
    }


    static BigNumber LongMulOneDigit(const BigNumber& A, uint32_t b) {
        BigNumber C;
        uint64_t carry = 0;
        for (int i = 0; i < n; ++i) {
            uint64_t temp = static_cast<uint64_t>(A.blocks[i]) * b + carry;
            C.blocks[i] = temp & UINT32_MAX;
            carry = temp >> 32;
        }
        return C;
    }

    static void LongShiftDigitsToHigh(BigNumber& A, int shift) {
        if (shift <= 0) return;
        for (int i = n - 1; i >= shift; --i) {
            A.blocks[i] = A.blocks[i - shift];
        }
        for (int i = 0; i < shift; ++i) {
            A.blocks[i] = 0;
        }
    }

    BigNumber operator*(const BigNumber& B) const {
        BigNumber C;
        for (int i = 0; i < n; ++i) {
            BigNumber temp = LongMulOneDigit(*this, B.blocks[i]);
            LongShiftDigitsToHigh(temp, i);
            C = C + temp;
        }
        return C;
    }


    int BitLength() const {
        int bitLength = 0;
        for (int i = n - 1; i >= 0; --i) {
            if (blocks[i] == 0) continue;
            int currentBlockBitLength = 32 - custom_clz(blocks[i]);
            bitLength = i * 32 + currentBlockBitLength;
            break;
        }
        return bitLength;
    }


    static void LongShiftBitsToHigh(BigNumber& A, int shift) {
        if (shift <= 0) return;

        int blockShift = shift / 32;
        int bitShift = shift % 32;

        for (int i = n - 1; i >= blockShift; --i) {
            A.blocks[i] = A.blocks[i - blockShift] << bitShift;
            if (i > blockShift && bitShift != 0) {
                A.blocks[i] |= A.blocks[i - blockShift - 1] >> (32 - bitShift);
            }
        }
        for (int i = 0; i < blockShift; ++i) {
            A.blocks[i] = 0;
        }
    }


    static void LongDivMod(const BigNumber& A, const BigNumber& B, BigNumber& Q, BigNumber& R) {
        if (LongCmp(B, BigNumber()) == 0) {
            std::cerr << "Error. Division by 0!\n";
            return;
        }

        int k = B.BitLength();
        R = A;
        Q = BigNumber(0u);
        while (LongCmp(R, B) >= 0) {
            int t = R.BitLength();
            BigNumber C = B;
            LongShiftBitsToHigh(C, t - k);


            if (LongCmp(R, C) < 0) {
                t = t - 1;
                C = B;
                LongShiftBitsToHigh(C, t - k);
            }

            R = R - C;

            BigNumber bitSetter;
            bitSetter.blocks[(t - k) / 32] = 1u << ((t - k) % 32);
            Q = Q + bitSetter;
        }
    }


    BigNumber operator/(const BigNumber& B) const {
        BigNumber Q, R;
        LongDivMod(*this, B, Q, R);
        return Q;
    }
    BigNumber operator%(const BigNumber& B) const {
        BigNumber Q, R;
        LongDivMod(*this, B, Q, R);
        return R;
    }
    

    BigNumber LongPower(const BigNumber& exponent) {
        BigNumber result(1);
        BigNumber A = *this;
        int bitLength = exponent.BitLength();

        for (int i = 0; i < bitLength; ++i) {
            int block_index = i / 32;
            int bit_position = i % 32;

            uint32_t bit_i = (exponent.blocks[block_index] >> bit_position) & 1;

            if (bit_i == 1) {
                result = result * A;
            }

            if (i != bitLength - 1) {
                A = A * A;
            }
        }

        return result;
    }



    std::string ToString(int base = 10) const {
        if (base != 2 && base != 10 && base != 16) {
            std::cerr << "Unsupported base\n";
            return "";
        }

        if (LongCmp(*this, BigNumber()) == 0) {
            return "0";
        }

        std::string result;
        std::string characters = "0123456789ABCDEF";
        BigNumber num = *this;
        BigNumber zero(0u);
        BigNumber baseBig(base);

        while (LongCmp(num, zero) != 0) {
            BigNumber remainder = num % baseBig;
            result = characters[remainder.blocks[0]] + result;
            num = num / baseBig;
        }

        return result;
    }

    void print() const {
        for (int i = n - 1; i >= 0; --i) {
            std::cout << blocks[i] << " ";
        }
        std::cout << std::endl;
    }
};


void testAddition() {
    // Test Case 1: A = 10101010101010101010, B = 10010010010010010010, Expected 100111100111100111100
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    assert(BigNumber::LongCmp(A1 + B1, BigNumber("100111100111100111100", 2)) == 0);
    // Test Case 2: A = 913784127983581, B = 699050, Expected 913784128682631
    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    assert(BigNumber::LongCmp(A2 + B2, BigNumber("913784128682631")) == 0);
    // Test Case 3: A = 12359182765471236582365123954133, B = 912730912344631278354412341235 , Expected 13271913677815867860719536295368
    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235");
    assert(BigNumber::LongCmp(A3 + B3, BigNumber("13271913677815867860719536295368")) == 0);
    // Test Case 4: A = AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA, B = CCCCCCCCCCCCC, Expected AAAAAAAAAAAAAAAAAAAAAAAAAAAB7777777777776
    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    assert(BigNumber::LongCmp(A4 + B4, BigNumber("AAAAAAAAAAAAAAAAAAAAAAAAAAAB7777777777776", 16)) == 0);
    // Test Case 5: A = ABCDEFABCEDFEACBDFEACABCDEFABCDEF, B = ABCDFAFACBACFACBACFACBACFACB , Expected abcdfa68ae8f9786af97858c8bb58c8ba
    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    assert(BigNumber::LongCmp(A5 + B5, BigNumber("ABCDFA68AE8F9786AF97858C8BB58C8BA", 16)) == 0);
   

}

void testSubtraction() {
    // Test Case 1: A = 10101010101010101010, B = 10010010010010010010, Expected 11000011000011000
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    assert(BigNumber::LongCmp(A1 - B1, BigNumber("11000011000011000", 2)) == 0);
    // Test Case 2: A = 913784127983581, B = 699050, Expected 913784127284531
    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    assert(BigNumber::LongCmp(A2 - B2, BigNumber("913784127284531")) == 0);
    // Test Case 3: A = 12359182765471236582365123954133, B = 912730912344631278354412341235 , Expected 11446451853126605304010711612898
    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235");
    assert(BigNumber::LongCmp(A3 - B3, BigNumber("11446451853126605304010711612898")) == 0);
    // Test Case 4: A = AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA, B = CCCCCCCCCCCCC, Expected AAAAAAAAAAAAAAAAAAAAAAAAAAA9DDDDDDDDDDDDE
    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    assert(BigNumber::LongCmp(A4 - B4, BigNumber("AAAAAAAAAAAAAAAAAAAAAAAAAAA9DDDDDDDDDDDDE", 16)) == 0);
    // Test Case 5: A = ABCDEFABCEDFEACBDFEACABCDEFABCDEF, B = ABCDFAFACBACFACBACFACBACFACB , Expected ABCDE4EEEF303E11103E0FED323FED324
    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    assert(BigNumber::LongCmp(A5 - B5, BigNumber("ABCDE4EEEF303E11103E0FED323FED324", 16)) == 0);

}

void testMultiplication() {
    // Test Case 1: A = 10101010101010101010, B = 10010010010010010010, Expected 110000110000110000011110011110011110100
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    assert(BigNumber::LongCmp(A1 * B1, BigNumber("110000110000110000011110011110011110100", 2)) == 0);
    // Test Case 2: A = 913784127983581, B = 699050, Expected 638780794666922298050
    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    assert(BigNumber::LongCmp(A2 * B2, BigNumber("638780794666922298050")) == 0);
    // Test Case 3: A = 12359182765471236582365123954133, B = 912730912344631278354412341235 , Expected 11280608161362604831470056138931627065443872666169257284574255
    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235");
    assert(BigNumber::LongCmp(A3 * B3, BigNumber("11280608161362604831470056138931627065443872666169257284574255")) == 0);
    // Test Case 4: A = AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA, B = CCCCCCCCCCCCC, Expected 8888888888887FFFFFFFFFFFFFFFFFFFFFFFFFFFF7777777777778
    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    assert(BigNumber::LongCmp(A4 * B4, BigNumber("8888888888887FFFFFFFFFFFFFFFFFFFFFFFFFFFF7777777777778", 16)) == 0);
    // Test Case 5: A = ABCDEFABCEDFEACBDFEACABCDEFABCDEF, B = ABCDFAFACBACFACBACFACBACFACB , Expected 734CCB701B9E74ADF6B697ED7A5A77404375BFBC867AC7CC64B7BB5EBB285
    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    assert(BigNumber::LongCmp(A5 * B5, BigNumber("734CCB701B9E74ADF6B697ED7A5A77404375BFBC867AC7CC64B7BB5EBB285", 16)) == 0);

}

void testDivision() {
    // Test Case 1: A = 10101010101010101010, B = 10010010010010010010, Expected 1
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    assert(BigNumber::LongCmp(A1 / B1, BigNumber("1", 2)) == 0);
    // Test Case 2: A = 913784127983581, B = 699050, Expected 1307179927
    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    assert(BigNumber::LongCmp(A2 / B2, BigNumber("1307179927")) == 0);
    // Test Case 3: A = 12359182765471236582365123954133, B = 912730912344631278354412341235 , Expected 13
    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235");
    assert(BigNumber::LongCmp(A3 / B3, BigNumber("13")) == 0);
    // Test Case 4: A = AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA, B = CCCCCCCCCCCCC, Expected D5555555555562AAAAAAAAAAAB80
    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    assert(BigNumber::LongCmp(A4 / B4, BigNumber("D5555555555562AAAAAAAAAAAB80", 16)) == 0);
    // Test Case 5: A = ABCDEFABCEDFEACBDFEACABCDEFABCDEF, B = ABCDFAFACBACFACBACFACBACFACB , Expected FFFFE
    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    assert(BigNumber::LongCmp(A5 / B5, BigNumber("FFFFE", 16)) == 0);

}

void testMod() {
    // Test Case 1: A = 10101010101010101010, B = 10010010010010010010, Expected 11000011000011000
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    assert(BigNumber::LongCmp(A1 % B1, BigNumber("11000011000011000", 2)) == 0);
    // Test Case 2: A = 913784127983581, B = 699050, Expected 14231
    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    assert(BigNumber::LongCmp(A2 % B2, BigNumber("14231")) == 0);
    // Test Case 3: A = 12359182765471236582365123954133, B = 912730912344631278354412341235 , Expected 493680904991029963757763518078
    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235");
    assert(BigNumber::LongCmp(A3 % B3, BigNumber("493680904991029963757763518078")) == 0);
    // Test Case 4: A = AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA, B = CCCCCCCCCCCCC, Expected AA
    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    assert(BigNumber::LongCmp(A4 % B4, BigNumber("AA", 16)) == 0);
    // Test Case 5: A = ABCDEFABCEDFEACBDFEACABCDEFABCDEF, B = ABCDFAFACBACFACBACFACBACFACB , Expected A2AC2924975D24974AF3DA55C385
    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    assert(BigNumber::LongCmp(A5 % B5, BigNumber("A2AC2924975D24974AF3DA55C385", 16)) == 0);

}

void testPower() {
    // Test Case 1: A = 10101010101010101010, B = 10010010010010010010, Expected 111000111000111000011100011100011100100
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10", 2);
    assert(BigNumber::LongCmp(A1.LongPower(B1), BigNumber("111000111000111000011100011100011100100", 2)) == 0);
    // Test Case 2: A = 913784127983581, B = 699050, Expected 913784128682631
    BigNumber A2("913784127983581");
    BigNumber B2("99");
    assert(BigNumber::LongCmp(A2.LongPower(B2), BigNumber("132897685776095433307693315519274741734029705099869463694991659680749984337220824357456772815026020473815965108328968853705128426643781536620724510097633955635065022672013900532582911536945818010608268984192115509757337682975414320386321099000453324022991687985926601073039239554391254166559266586104405032501374695519929782030511303565168220682073839257866935361937137580777212664265298556729083211869821189665112094924707863613256836535867045761214868844948181760945762008186945339547565152633633763274166826281562331642928221255607150744993686587827408386902437643109859263401897140551836633891545976703693473356913842313342625885677865060475654566141258039798089005362529674282897470614703949093688438289856529538348880958223696255825335622015721005714484873015876997574820528800691464165885740059155368860544207945932472436260141072730470483547883128633177378183533717736269230497584106474744479742329859702308485649582883024119911803180586739846417139380978723555846895208433894572155281375561574647565737556452552754955791063329939173059825360047996357005711487111568823579566751758387771322973857924348942905540234243970266433513868226863969601907347259583201499035501181353302698336627437980581149191679587098779591268730695355520655276878638302568010640967790633059109986446247331817115996192791536415494834358756017955253474658102637188563435137271621297449204170828356204822781830622215944892573022738707177787407819057974154965112237267405365333049861571046326590178821")) == 0);
    // Test Case 3: A = 12359182765471236582365123954133, B = 912730912344631278354412341235 , Expected 
    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("3");
    assert(BigNumber::LongCmp(A3.LongPower(B3), BigNumber("1887857734987962021413293547583248227696757515712481530421211303918452244204791477749153270637")) == 0);
    // Test Case 4: A = AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA, B = CCCCCCCCCCCCC, Expected 1F91BD1B62B9CEC8A5751C585FD645BDAAED61A735DCFF801EA4F81115555D39C49C2E03C9077EB2A48F000BF638A76068675A06B15FBD7C50306CDDC23FEC7C28D9D238A3EC77B85B3A5EC1CB638116C29F5F1DE9A64FFFD690B7CC0E6C2098A6D64ACC0230A9C8FEDCD8E6EE6E7C5BE7820AED94F420D6CDC30AA86694B7D374947303CA62C1D2C9A9692D85E0C22DCBCC92202186E6B57579147B56B7BF7A5AC7AF8DD8B3820972C016D836181130F4E2B1BAFEC2F3581BE397B7B96F7595716B3D52726A230C5688454CFCBBC8A1E199EC45A73950F02F1D714AAF076E2C53576A4FCFFDAE1A4A6CCEB87DE15754DAC17511000
    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("C", 16);
    assert(BigNumber::LongCmp(A4.LongPower(B4), BigNumber("1F91BD1B62B9CEC8A5751C585FD645BDAAED61A735DCFF801EA4F81115555D39C49C2E03C9077EB2A48F000BF638A76068675A06B15FBD7C50306CDDC23FEC7C28D9D238A3EC77B85B3A5EC1CB638116C29F5F1DE9A64FFFD690B7CC0E6C2098A6D64ACC0230A9C8FEDCD8E6EE6E7C5BE7820AED94F420D6CDC30AA86694B7D374947303CA62C1D2C9A9692D85E0C22DCBCC92202186E6B57579147B56B7BF7A5AC7AF8DD8B3820972C016D836181130F4E2B1BAFEC2F3581BE397B7B96F7595716B3D52726A230C5688454CFCBBC8A1E199EC45A73950F02F1D714AAF076E2C53576A4FCFFDAE1A4A6CCEB87DE15754DAC17511000", 16)) == 0);
    // Test Case 5: A = ABCDEFABCEDFEACBDFEACABCDEFABCDEF, B = ABCDFAFACBACFACBACFACBACFACB , Expected 4BE9301CE9F8F754BA4A8A24E7E4524EE79124FD4FAE2DACC9B29771F139DE3F26C4E73A796C92F171DC950151163964B689A65F35E57C11E0CFDF387D0FE8DC2B4B59238A0A2F9734E225C47A8E117D0DFD624C9F93F3F748B30446BDABB35A0680740CDE8B6F7408B61BB4999E857881A820125D31E983A648BF05821F68CA8963809283B69AB6737EE9F51FBDD43B7DD31F01467EFBA7B9A73DB7CD23909969040E1A1
    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("A", 16);
    assert(BigNumber::LongCmp(A5.LongPower(B5), BigNumber("4BE9301CE9F8F754BA4A8A24E7E4524EE79124FD4FAE2DACC9B29771F139DE3F26C4E73A796C92F171DC950151163964B689A65F35E57C11E0CFDF387D0FE8DC2B4B59238A0A2F9734E225C47A8E117D0DFD624C9F93F3F748B30446BDABB35A0680740CDE8B6F7408B61BB4999E857881A820125D31E983A648BF05821F68CA8963809283B69AB6737EE9F51FBDD43B7DD31F01467EFBA7B9A73DB7CD23909969040E1A1", 16)) == 0);

}

void otherTests() {
    BigNumber a("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16);
    BigNumber b("1010101010101011111111111111111111111111111111111111111", 2);
    BigNumber c("386293478126867451827365412783456712");
    BigNumber ans1 = (a + b) * c;
    BigNumber ans2 = c * (a + b);
    BigNumber ans3 = a * c + c * b;
    assert(BigNumber::LongCmp(ans1, ans2) == 0);
    assert(BigNumber::LongCmp(ans1, ans3) == 0);
    assert(BigNumber::LongCmp(ans2, ans3) == 0);
    
    int n = 1337;
    BigNumber ans4;
    for (int i = 0; i < n; i++) {
        ans4 = ans4 + a;
    }
    assert(BigNumber::LongCmp(ans4, a*n) == 0);
}




std::string generateRandomNumberString(int length) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, 9); // generating digits between 0 and 9

    std::string result;
    for (int i = 0; i < length; ++i) {
        result += std::to_string(dis(gen));
    }
    return result;
}

void timeTest() {

    BigNumber numbers[1001];
    for (int i = 0; i < 1001; i++) {
        BigNumber temp(generateRandomNumberString(100));
        numbers[i] = temp;
    }
    BigNumber C;
    auto start1 = std::chrono::high_resolution_clock::now();
    unsigned __int64 start_tick1 = __rdtsc();
    for (int i = 1; i < 1001; i++) {
        C = numbers[i] + numbers[i - 1];
    }
    unsigned __int64 end_tick1 = __rdtsc();
    unsigned __int64 ticks1 = end_tick1 - start_tick1;
    auto stop1 = std::chrono::high_resolution_clock::now();
    auto duration1 = std::chrono::duration_cast<std::chrono::microseconds>(stop1 - start1);
    std::cout << "Time taken for addition: " << duration1.count() << " microseconds. Ticks: " << ticks1 << std::endl;


    auto start2 = std::chrono::high_resolution_clock::now();
    unsigned __int64 start_tick2 = __rdtsc();
    for (int i = 1; i < 1001; i++) {
        C = numbers[i] - numbers[i - 1];
    }
    unsigned __int64 end_tick2 = __rdtsc();
    unsigned __int64 ticks2 = end_tick2 - start_tick2;
    auto stop2 = std::chrono::high_resolution_clock::now();
    auto duration2 = std::chrono::duration_cast<std::chrono::microseconds>(stop2 - start2);
    std::cout << "Time taken for substriction: " << duration2.count() << " microseconds. Ticks: " << ticks2 << std::endl;



    auto start3 = std::chrono::high_resolution_clock::now();
    unsigned __int64 start_tick3 = __rdtsc();
    for (int i = 1; i < 1001; i++) {
        C = numbers[i] * numbers[i - 1];
    }
    unsigned __int64 end_tick3 = __rdtsc();
    unsigned __int64 ticks3 = end_tick3 - start_tick3;
    auto stop3 = std::chrono::high_resolution_clock::now();
    auto duration3 = std::chrono::duration_cast<std::chrono::microseconds>(stop3 - start3);
    std::cout << "Time taken for multiplication: " << duration3.count() << " microseconds. Ticks: " << ticks3 << std::endl;



    auto start4 = std::chrono::high_resolution_clock::now();
    unsigned __int64 start_tick4 = __rdtsc();
    for (int i = 1; i < 1001; i++) {
        C = numbers[i] / numbers[i - 1];
    }
    unsigned __int64 end_tick4 = __rdtsc();
    unsigned __int64 ticks4 = end_tick4 - start_tick4;
    auto stop4 = std::chrono::high_resolution_clock::now();
    auto duration4 = std::chrono::duration_cast<std::chrono::microseconds>(stop4 - start4);
    std::cout << "Time taken for division: " << duration4.count() << " microseconds. Ticks: " << ticks4 << std::endl;
}


int main() {
    testAddition();
    testSubtraction();
    testMultiplication();
    testDivision();
    testMod();
    testPower();
    otherTests();
    std::cout << "All tests passed.\n";

    timeTest();

    return 0;
}


