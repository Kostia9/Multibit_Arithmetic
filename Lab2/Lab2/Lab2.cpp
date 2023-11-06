#include <iostream>
#include <cassert>
#include <chrono>
#include <random>
#include <string>
#include <intrin.h>


//Count Leading Zeros
int custom_clz(uint32_t x) {
    if (x == 0) return 32; // якщо чиcло 0, повертаємо 32 (вcі біти нульові)

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
    static const int n = 65; // 2048 bit
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
        if (this != &other) {  // перевірка на присвоювання самому собі
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
            return BigNumber(0u); // повертаємо 0
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
        BigNumber temp;

        for (int i = 0; i < n; ++i) {
            if (B.blocks[i] == 0) continue; // Ігноруємо множення, якщо поточний блок рівний нулю

            temp = LongMulOneDigit(*this, B.blocks[i]);
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

    static void LongShiftBitsToLow(BigNumber& A, int shift) {
        if (shift <= 0) return;

        int blockShift = shift / 32;
        int bitShift = shift % 32;

        for (int i = 0; i < n - blockShift; ++i) {
            A.blocks[i] = A.blocks[i + blockShift] >> bitShift;
            if (i + blockShift + 1 < n && bitShift != 0) {
                A.blocks[i] |= A.blocks[i + blockShift + 1] << (32 - bitShift);
            }
        }
        for (int i = n - blockShift; i < n; ++i) {
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

            BigNumber bitSetter; //встановити в Q біт із номером(t – k)
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
    BigNumber operator>>(const int shift) const {
        if (shift < 0) {
            std::cerr << "Negative shift is not supported." << std::endl;
            return *this;
        }
        BigNumber A = *this;
        LongShiftBitsToLow(A, shift);
        return A;
    }
    BigNumber operator<<(const int shift) const {
        if (shift < 0) {
            std::cerr << "Negative shift is not supported." << std::endl;
            return *this;
        }
        BigNumber A = *this;
        LongShiftBitsToHigh(A, shift);
        return A;
    }
    BigNumber LongPower(const BigNumber& exponent) const {

        BigNumber C(1);
        BigNumber A = *this;
        int bitLength = exponent.BitLength();
        for (int i = bitLength - 1; i >= 0; --i) {

            uint32_t bit_i = exponent.GetBit(i);
            if (bit_i == 1) {
                C = C * A;
            }
            if (i != 0) {
                C = C * C;
            }
        }
        return C;
    }
    bool operator==(const BigNumber& B) const {
        return LongCmp(*this, B) == 0;
    }
    bool operator!=(const BigNumber& B) const {
        return LongCmp(*this, B) != 0;
    }
    bool operator>(const BigNumber& B) const {
        return LongCmp(*this, B) == 1;
    }
    bool operator>=(const BigNumber& B) const {
        return LongCmp(*this, B) >= 0;
    }
    bool operator<(const BigNumber& B) const {
        return LongCmp(*this, B) == -1;
    }
    bool operator<=(const BigNumber& B) const {
        return LongCmp(*this, B) <= 0;
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

    std::string ToHexString() const {
        std::string result;
        std::string hexDigits = "0123456789ABCDEF";

        for (int blockIndex = n - 1; blockIndex >= 0; --blockIndex) {
            uint32_t block = blocks[blockIndex];
            for (int hexIndex = 0; hexIndex < 8; ++hexIndex) {
                
                uint32_t hexDigit = (block >> (28 - hexIndex * 4)) & 0xF;
                result += hexDigits[hexDigit];
            }
        }
        // Обрізати нулі на початку, якщо вони є
        auto firstNonZero = result.find_first_not_of('0');
        if (firstNonZero != std::string::npos) {
            result.erase(0, firstNonZero);
        }
        else {
            // Усе число було нулями
            result = "0";
        }
        return result;
    }
    std::string ToBinaryString() const {
        std::string result;
        for (int blockIndex = n - 1; blockIndex >= 0; --blockIndex) {
            uint32_t block = blocks[blockIndex];
            for (int bitIndex = 31; bitIndex >= 0; --bitIndex) {
                result += ((block >> bitIndex) & 1) ? '1' : '0';
            }
        }
        // Обрізати нулі на початку, якщо вони є
        auto firstNonZero = result.find_first_not_of('0');
        if (firstNonZero != std::string::npos) {
            result.erase(0, firstNonZero);
        }
        else {
            // Усе число було нулями
            result = "0";
        }
        return result;
    }

    void print() const {
        for (int i = n - 1; i >= 0; --i) {
            std::cout << blocks[i] << " ";
        }
        std::cout << std::endl;
    }


    //Lab2
    bool IsEven() const {
        return (blocks[0] & 1) == 0;
    }
    int DigitLength() const{
        for (int i = n - 1; i >= 0; --i) {
            if (blocks[i] != 0) {
                return i + 1; // Повертаємо кількість блоків
            }
        }
        return 0; 
    }
    int GetBit(int i) const{
        int block_index = i / 32;
        int bit_position = i % 32;
        return ((*this).blocks[block_index] >> bit_position) & 1;
    }
    int GetDigit(int i) const {
        return (*this).blocks[i];
    }

};

//Lab2
BigNumber gcd(BigNumber A, BigNumber B) {
    BigNumber D(1u);

    while (A.IsEven() && B.IsEven()) {
        A = A >> 1;
        B = B >> 1;
        D = D << 1;
    }
    while (A.IsEven()) {
        A = A >> 1;
    }
    while (B > 0u) { 
        while (B.IsEven()) {
            B = B >> 1;
        }
        if (A > B) {
            //(a, b) := (min{a, b}, abs(a – b))
            BigNumber temp = A;  
            A = B;  
            B = temp - B;
        }
        else {
            B = B - A;
        }
    }
    D = D * A;
    return D;
}

BigNumber lcm(BigNumber A, BigNumber B) {
    BigNumber divisor = gcd(A, B);
    A = A / divisor; // Divide first to avoid overflow
    return A * B;
}

BigNumber CalculateMu(const BigNumber& n) {
    int k = n.DigitLength();
    BigNumber betaTo2k = BigNumber(1);

    // Зсув на 2k бітів вліво, враховуючи, що β = 2^32
    betaTo2k = betaTo2k << (32 * 2 * k);

    BigNumber mu = betaTo2k / n;
    return mu;
}

BigNumber BarrettReduction(const BigNumber& x, const BigNumber& n, const BigNumber& mu) {
    int k = n.DigitLength();
    BigNumber temp = n;

    // Зсув на (k-1) бітів вправо, враховуючи, що β = 2^32
    BigNumber q = x >> (32 * (k - 1));

    q = q * mu;

    // Зсув на (k+1) бітів вправо, враховуючи, що β = 2^32
    q = q >> (32 * (k + 1));

    BigNumber r = x - (q * n);

    int max_iterations = 1000;

    while (r >= n) {
        r = r - n;
        if (!(max_iterations--)) {
            std::cerr << "Error: Loop executed more than 1000 times.";
            exit(-1);
        }
    }
    return r;
}

BigNumber ModAddBarrett(const BigNumber& a, const BigNumber& b, const BigNumber& mod, BigNumber mu = 0u) {
    if (mu == 0u) {
        mu = CalculateMu(mod);
    }
    return BarrettReduction((a + b), mod, mu);
}

BigNumber ModSubBarrett(const BigNumber& A, const BigNumber& B, const BigNumber& mod, BigNumber mu = 0u) {
    if (mu == 0u) {
        mu = CalculateMu(mod);
    }
    BigNumber modA = BarrettReduction(A, mod, mu);
    BigNumber modB = BarrettReduction(B, mod, mu);

    BigNumber result;
    if (modB > modA) {
        result = (modA + mod) - modB;
    }
    else {
        result = modA - modB;
    }

    return result;
}

BigNumber ModMulBarrett(const BigNumber& A, const BigNumber& B, const BigNumber& mod, BigNumber mu = 0u) {
    if (mu == 0u) {
        mu = CalculateMu(mod);
    }
    BigNumber modA = BarrettReduction(A, mod, mu);
    BigNumber modB = BarrettReduction(B, mod, mu);
    BigNumber C = modA * modB;
    return BarrettReduction(C, mod, mu);
}

BigNumber ModLongPowBarrett(BigNumber A, const BigNumber& B, const BigNumber& mod) {
    BigNumber C(1u);
    BigNumber mu = CalculateMu(mod);
    int bitLength = B.BitLength();

    A = BarrettReduction(A, mod, mu);
    for (int i = 0; i < bitLength; i++) {
        if (B.GetBit(i) == 1) {
            C = BarrettReduction(C * A, mod, mu);
        }       
        A = BarrettReduction(A * A, mod, mu);
    }
    return C;
}

BigNumber ModWindowPow(const BigNumber& base, const BigNumber& exponent, const BigNumber& mod, int window=4) {

    int windowSize = window;

    BigNumber mu = CalculateMu(mod);
    int numPrecomputed = 1 << windowSize; // 2^windowSize
    std::vector<BigNumber> precomputed(numPrecomputed);

    BigNumber mod_base = BarrettReduction(base, mod, mu);
    precomputed[0] = BigNumber(1u);
    precomputed[1] = mod_base;
    for (int i = 2; i < numPrecomputed; ++i) {
        precomputed[i] = BarrettReduction(precomputed[i - 1] * mod_base, mod, mu);
    }


    BigNumber result(1u);

    int bitLength = exponent.BitLength();
    for (int i = bitLength - bitLength % windowSize; i >= 0; i -= windowSize) {
        // Square result windowSize times
        for (int j = 0; j < windowSize; ++j) {
            result = BarrettReduction(result * result, mod, mu);
        }

        int b = 0;
        for (int k = 0; k < windowSize; ++k) {
            if (exponent.GetBit(i + k)) {
                b |= (1 << k);
            }
        }
        // Multiply by the precomputed value
        if (b != 0) {
            result = BarrettReduction(result * precomputed[b], mod, mu);
        }
    }

    return result;
}


void testgcd() {
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    assert(gcd(A1, B1) == BigNumber("10", 2));

    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    assert(gcd(A2, B2) == BigNumber("1"));

    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235");
    assert(gcd(A3, B3) == BigNumber("1"));

    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    assert(gcd(A4, B4) == BigNumber("2", 16));

    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    assert(gcd(A5, B5) == BigNumber("1", 16));
}

void testlcm() {
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    assert(lcm(A1, B1) == BigNumber("11000011000011000001111001111001111010", 2));

    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    assert(lcm(A2, B2) == BigNumber("638780794666922298050"));

    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235");
    assert(lcm(A3, B3) == BigNumber("11280608161362604831470056138931627065443872666169257284574255"));

    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    assert(lcm(A4, B4) == BigNumber("4444444444443FFFFFFFFFFFFFFFFFFFFFFFFFFFFBBBBBBBBBBBBC", 16));

    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    assert(lcm(A5, B5) == BigNumber("734CCB701B9E74ADF6B697ED7A5A77404375BFBC867AC7CC64B7BB5EBB285", 16));
}

void testModAddition() {
    
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    BigNumber mod1("111010101010", 2);
    assert(ModAddBarrett(A1, B1, mod1, CalculateMu(mod1)) == BigNumber("110000100010", 2));

    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    BigNumber mod2("123456789");
    assert(ModAddBarrett(A2, B2, mod2, CalculateMu(mod2)) == BigNumber("62923992"));

    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235");
    BigNumber mod3("123456789101112134");
    assert(ModAddBarrett(A3, B3, mod3, CalculateMu(mod3)) == BigNumber("105603347056183678"));

    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    BigNumber mod4("12345678910111213ABC4", 16);
    assert(ModAddBarrett(A4, B4, mod4, CalculateMu(mod4)) == BigNumber("583B5A849FD245E0A92E", 16));

    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    BigNumber mod5("12345678910111213ABC4", 16);
    assert(ModAddBarrett(A5, B5, mod5, CalculateMu(mod5)) == BigNumber("11A64B04C24DD4F4B7A02", 16));
}

void testModSubtraction() {
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    BigNumber mod1("111010101010", 2);
    assert(ModSubBarrett(A1, B1, mod1, CalculateMu(mod1)) == BigNumber("100011010100", 2));

    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    BigNumber mod2("123456789");
    assert(ModSubBarrett(B2, A2, mod2, CalculateMu(mod2)) == BigNumber("61930897"));

    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235812643");
    BigNumber mod3("1234567891011121342");
    assert(ModSubBarrett(A3, B3, mod3, CalculateMu(mod3)) == BigNumber("955161601221964956"));

    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    BigNumber mod4("12345678910111213ABC4", 16);
    assert(ModSubBarrett(A4, B4, mod4, CalculateMu(mod4)) == BigNumber("583B5A6B0638AC470F96", 16));

    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    BigNumber mod5("12345678910111213ABC4", 16);
    assert(ModSubBarrett(A5, B5, mod5, CalculateMu(mod5)) == BigNumber("8DE13D5116E0932641E0", 16));

}

void testModMultiplication() {
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    BigNumber mod1("111010101010", 2);
    assert(ModMulBarrett(A1, B1, mod1, CalculateMu(mod1)) == BigNumber("110000000110", 2));

    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    BigNumber mod2("123456789");
    assert(ModMulBarrett(A2, B2, mod2, CalculateMu(mod2)) == BigNumber("74495996"));

    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235812643");
    BigNumber mod3("1234567891011121342");
    assert(ModMulBarrett(A3, B3, mod3, CalculateMu(mod3)) == BigNumber("616594869900571877"));

    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    BigNumber mod4("12345678910111213ABC4", 16);
    assert(ModMulBarrett(A4, B4, mod4, CalculateMu(mod4)) == BigNumber("843689BCB267B1B3E138", 16));

    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    BigNumber mod5("12345678910111213ABC4", 16);
    assert(ModMulBarrett(A5, B5, mod5, CalculateMu(mod5)) == BigNumber("122ACDE35BCCD2DABC229", 16));

}

void testModPower() {
    BigNumber A1("10101010101010101010", 2);
    BigNumber B1("10010010010010010010", 2);
    BigNumber mod1("111010101010", 2);
    assert(ModLongPowBarrett(A1, B1, mod1) == BigNumber("11010100010", 2));

    BigNumber A2("913784127983581");
    BigNumber B2("699050");
    BigNumber mod2("123456789");
    assert(ModLongPowBarrett(A2, B2, mod2) == BigNumber("76332175"));

    BigNumber A3("12359182765471236582365123954133");
    BigNumber B3("912730912344631278354412341235812643");
    BigNumber mod3("1234567891011121342");
    assert(ModLongPowBarrett(A3, B3, mod3) == BigNumber("1148493446967218319"));

    BigNumber A4("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 16);
    BigNumber B4("CCCCCCCCCCCCC", 16);
    BigNumber mod4("12345678910111213ABC4", 16);
    assert(ModWindowPow(A4, B4, mod4) == BigNumber("2F62B5C1F2D44A1A0704", 16));

    BigNumber A5("ABCDEFABCEDFEACBDFEACABCDEFABCDEF", 16);
    BigNumber B5("ABCDFAFACBACFACBACFACBACFACB", 16);
    BigNumber mod5("12345678910111213ABC4", 16);
    assert(ModWindowPow(A5, B5, mod5) == BigNumber("185C80771CA4B79FE1BB", 16));
}

void otherModTests() {
    BigNumber a("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16);
    BigNumber b("1010101010101011111111111111111111111111111111111111111", 2);
    BigNumber c("386293478126867451827365412783456712");
    BigNumber mod("1111111111111111111111111111111");
    BigNumber mu = CalculateMu(mod);
    BigNumber ans1 = ModMulBarrett(ModAddBarrett(a, b, mod, mu), c, mod, mu);
    BigNumber ans2 = ModMulBarrett(c, ModAddBarrett(a, b, mod, mu), mod, mu);
    BigNumber ans3 = ModAddBarrett(ModMulBarrett(a, c, mod, mu), ModMulBarrett(c, b, mod, mu), mod, mu);
    assert(ans1 == ans2);
    assert(ans1 == ans3);
    assert(ans2 == ans3);
    int n = 1337;
    BigNumber ans4(0u);
    for (int i = 0; i < n; i++) {
        ans4 = ModAddBarrett(ans4, a, mod, mu);
    }
    assert(ans4 == ModMulBarrett(a, n, mod, mu));
    BigNumber phin("1110712846122408779600685764760");
    BigNumber aphin = ModLongPowBarrett(a, phin, mod);
    BigNumber bphin = ModLongPowBarrett(b, phin, mod);
    BigNumber cphin = ModLongPowBarrett(c, phin, mod);

    assert(aphin == BigNumber(1));
    assert(bphin == BigNumber(1));
    assert(cphin == BigNumber(1));
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

    BigNumber numbers[20];
    for (int i = 0; i < 20; i++) {
        BigNumber temp(generateRandomNumberString(310));
        numbers[i] = temp;
    }
    BigNumber C;
    BigNumber mod("106996671871463540628827509979984302027760961738716914281479406021558536001927354795815954125248433647085073626643253515286261112914874177425735509880304306221171433878319461229386901217952817332947823059306949711818877357949933109058898188411808140452717418109591465018404825234582959291614727333118024592189");
    BigNumber mu = CalculateMu(mod);
    auto start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        gcd(numbers[i % 20], numbers[(i - 1) % 20]);
    }
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for gcd: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        lcm(numbers[i % 20], numbers[(i - 1) % 20]);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for lcm: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModAddBarrett(numbers[i % 20], numbers[(i - 1) % 20], mod, mu);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModAddBarrett: " << duration.count() << " microseconds." << std::endl;

    
    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModSubBarrett(numbers[i % 20], numbers[(i - 1) % 20], mod, mu);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModSubBarrett: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModMulBarrett(numbers[i % 20], numbers[(i - 1) % 20], mod, mu);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModMulBarrett: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModLongPowBarrett(numbers[i % 20], numbers[(i - 1) % 20], mod);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModLongPowBarrett: " << duration.count() << " microseconds." << std::endl;


}

void windowTest() {
    BigNumber numbers[20];
    for (int i = 0; i < 20; i++) {
        BigNumber temp(generateRandomNumberString(309));
        numbers[i] = temp;
    }
    BigNumber C;
    BigNumber mod("106996671871463540628827509979984302027760961738716914281479406021558536001927354795815954125248433647085073626643253515286261112914874177425735509880304306221171433878319461229386901217952817332947823059306949711818877357949933109058898188411808140452717418109591465018404825234582959291614727333118024592189");
    BigNumber mu = CalculateMu(mod);

    auto start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModLongPowBarrett(numbers[i % 20], numbers[(i - 1) % 20], mod);
    }
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModLongPowBarrett: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModWindowPow(numbers[i % 20], numbers[(i - 1) % 20], mod, 2);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModWindowPow2: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModWindowPow(numbers[i % 20], numbers[(i - 1) % 20], mod, 3);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModWindowPow3: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModWindowPow(numbers[i % 20], numbers[(i - 1) % 20], mod, 4);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModWindowPow4: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModWindowPow(numbers[i % 20], numbers[(i - 1) % 20], mod, 5);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModWindowPow5: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModWindowPow(numbers[i % 20], numbers[(i - 1) % 20], mod, 6);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModWindowPow6: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModWindowPow(numbers[i % 20], numbers[(i - 1) % 20], mod, 7);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModWindowPow7: " << duration.count() << " microseconds." << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 1; i < 101; i++) {
        ModWindowPow(numbers[i % 20], numbers[(i - 1) % 20], mod, 8);
    }
    stop = std::chrono::high_resolution_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    std::cout << "Time taken for ModWindowPow8: " << duration.count() << " microseconds." << std::endl;
}

int main() {
    testgcd();
    testlcm();
    testModSubtraction();
    testModAddition();
    otherModTests();
    testModMultiplication();
    testModPower();
    std::cout << "All test completed!\n";

    windowTest();

    return 0;
}