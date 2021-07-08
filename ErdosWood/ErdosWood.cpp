#include<iostream>
#include<set>
#include<vector>
#include<array>
#include<chrono>
#include<map>
#include<boost/multiprecision/cpp_int.hpp>
#include<thread>
#include<future>
#include<memory>
#include<string>
#include<stdexcept>
using namespace boost::multiprecision;
using namespace std;
int get_first(set<int> &x) {
    return x.size() ? *x.begin() : 0;
}

template<typename ... Args>
std::string string_format(const std::string& format, Args ... args)
{
    int size_s = std::snprintf(nullptr, 0, format.c_str(), args ...) + 1; // Extra space for '\0'
    if (size_s <= 0) { throw std::runtime_error("Error during formatting."); }
    auto size = static_cast<size_t>(size_s);
    auto buf = std::make_unique<char[]>(size);
    std::snprintf(buf.get(), size, format.c_str(), args ...);
    return std::string(buf.get(), buf.get() + size - 1); // We don't want the '\0' inside
}

void process_subset(vector<array<int1024_t, 2>> * prime_sets, set<int> * primes_both_sides, int x, size_t cur_thread, int num_threads, promise<int1024_t> * response) {
    int1024_t min_a_k = 0;
    size_t num_primes_both_sides = primes_both_sides->size();
    size_t size_primes_both_sides = (size_t)1 << num_primes_both_sides;
    size_t size_prime_sets = size_primes_both_sides * prime_sets->size();
    if (cur_thread == 0 && size_prime_sets > 100000) {
        if (size_prime_sets > 10000000) {
            cerr << "00.0% done\r";
        }
        else {
            cerr << "00% done\r";
        }
    }
    for (size_t i = cur_thread; i < size_primes_both_sides; i+=num_threads) {
        if (cur_thread == 0 && size_prime_sets > 100000) {
            if (size_prime_sets > 10000000) {
                if ((i / num_threads) % max((size_t)1, size_primes_both_sides / num_threads / 1000) == 0) {
                    cerr << setfill('0') << setw(4) << string_format("%0.1f", ((double)100) * i / size_primes_both_sides) << "\r";
                }
            }
            else if ((i / num_threads) % max((size_t)1, size_primes_both_sides / num_threads / 100) == 0) {
                cerr << setfill('0') << setw(2) << 100 * i / size_primes_both_sides << "\r";
            }
        }
        array<int1024_t, 2>prods_primes_both_sides = { 1,1 };
        auto k = primes_both_sides->begin();
        for (int j = 0; j < num_primes_both_sides; j++, k++) {
            prods_primes_both_sides[bit_test(i, j)] *= *k;
        }

        for (auto j = prime_sets->begin(); j != prime_sets->end(); j++) {
            int1024_t g0, g1, f0, f1;
            int64_t f2, f3;
            g0 = (*j)[0] * prods_primes_both_sides[0]; g1 = (*j)[1] * prods_primes_both_sides[1];
            vector<int1024_t>quotients;
            quotients.reserve(1000);
            vector<int64_t>quotients2;
            quotients2.reserve(1000);
            if (g0 > g1) { swap(g0, g1); }
            f0 = g0; f1 = g1;
            while (f0 > 1 && (f0 >= INT32_MAX / 2 || f1 >= INT32_MAX / 2)) {
                int1024_t n0,n1;
                divide_qr(f1, f0, n1, n0);
                quotients.push_back(n1);
                f1 = f0;
                f0 = n0;
            }
            if (f0 > 1) {
                f2 = (int64_t)f0;
                f3 = (int64_t)f1;
                while (f2 > 1) {
                    int64_t n2,n3;
                    divide_qr(f3, f2, n3, n2);
                    quotients2.push_back(n3);
                    f3 = f2;
                    f2 = n2;
                }
            }
            f2 = 0; f3 = 1;
            for (auto k = quotients2.rbegin(); k != quotients2.rend(); k++) {
                int64_t n2 = f3;
                f3 = *k * f3 + f2;
                f2 = n2;
            }
            f0 = f2; f1 = f3;
            for (auto k = quotients.rbegin(); k != quotients.rend(); k++) {
                int1024_t n = f1;
                f1 = *k * f1 + f0;
                f0 = n;
            }
            int1024_t K = (x * f0 + g0 / 2) / g0;
            int1024_t o = min(abs(f1 * x - g1 * K) * g0, abs(f0 * x - g0 * K) * g1);
            if (!min_a_k || o < min_a_k) { min_a_k = o; }
        }
    }
        //cout<<endl;
    response->set_value(min_a_k);
}

int main()
{
    int num_threads = 8;
    double total_time = 0;
    for (int x = 16; x < 1000; x++) {
        auto start = chrono::system_clock::now();
        vector<array<set<int>, 2>>factor_pairs(x - 1), new_factor_pairs;
        vector<pair<array<int1024_t, 2>, int>>prime_set_bitmaps, new_bitmaps;
        vector<array<int1024_t, 2>>final_bitmaps;
        map<int, int>count_primes;

        set<int>primes_both_sides;
        int1024_t prod_unique_pfs_x = 1;
        int min_prime = 0;
        
        for (int i = 2; i < x; i ++) {
            if (!factor_pairs[i - 1][0].size()) {
                for (int j = 1; j < (x - 1) / i + 1; j++) {
                    factor_pairs[i * j - 1][0].insert(i);
                    factor_pairs[x - i * j - 1][1].insert(i);
                }
                if (x % i == 0) {
                    prod_unique_pfs_x *= i;
                }
                else if (!min_prime) {
                    min_prime = i;
                }
            }
        }
        if (factor_pairs[0][1].size() == 1) {
            continue;
        }

        for (auto i = factor_pairs.begin(); i != factor_pairs.end(); i++) {
            bool e = false;
            if ((*i)[0].size()) {
                for (auto j = (*i)[0].begin(); j != (*i)[0].end(); j++) {
                    if (prod_unique_pfs_x % *j == 0) {
                        e = true;
                        break;
                    }
                }
            }
            if (e) { continue; }
            if (get_first((*i)[0]) != min_prime) {
                new_factor_pairs.push_back(*i);
            }
            for (int j = 0; j < 2; j++) {
                for (auto k = (*i)[j].begin(); k != (*i)[j].end(); k++) {
                    auto ci = count_primes.find(*k);
                    if (ci == count_primes.end()) {
                        count_primes.insert(make_pair(*k, 1));
                    }
                    else {
                        ci->second++;
                    }
                }
            }
        }
        factor_pairs = new_factor_pairs;

        map<int, int>prime_bit_representations;
        vector<int>rev_prime_bit_representations;
        vector<array<pair<int1024_t,vector<unsigned>>,2>>bitmaps;
        int cn = 0;
        for (auto i = factor_pairs.begin(); i != factor_pairs.end(); i++) {
            if (((*i)[0].size() == 1 && (*i)[0].size() == 1 && count_primes[*((*i)[0].begin())] < 3 && count_primes[*((*i)[1].begin())] < 3)) {
                int z = *((*i)[0].begin());
                if (z != *((*i)[1].begin())) { z *= *((*i)[1].begin()); };
                if (!primes_both_sides.count(z)) {
                    primes_both_sides.insert(z);
                }
            }
            else {
                array<pair<int1024_t,vector<unsigned>>,2>bitmap;
                for (int j = 0; j < 2; j++) {
                    bitmap[j].first = 0;
                    for (auto k = (*i)[j].begin(); k != (*i)[j].end(); k++) {
                        auto pbr_it = prime_bit_representations.find(*k);
                        unsigned cur_index;
                        if (pbr_it == prime_bit_representations.end()) {
                            prime_bit_representations.insert(make_pair(*k, cn));
                            rev_prime_bit_representations.push_back(*k);
                            cur_index = cn;
                            cn++;
                        }
                        else {
                            cur_index = pbr_it->second;
                        }
                        bitmap[j].second.push_back(cur_index);
                        bitmap[j].first |= ((int1024_t)1) << cur_index;
                    }
                }
                bitmaps.push_back(bitmap);
            }
        }

        sort(bitmaps.begin(), bitmaps.end(), [](array<pair<int1024_t, vector<unsigned>>, 2>&x, array<pair<int1024_t, vector<unsigned>>, 2>&y) {
            return min(x[0].first, x[1].first) < min(y[0].first, y[1].first);
        });
        prime_set_bitmaps.push_back(make_pair(array<int1024_t, 2>{((int1024_t)1) << prime_bit_representations[min_prime], 0}, 0));
        // cout << "v size: " << v.size();
        //int ii = 0;
        bool done = false, done_second = false;
        while (prime_set_bitmaps.size() > 0 && prime_set_bitmaps.size() < 40000000) { // memory issues above this
            if (done && !done_second) {
                done_second = true;
            }
            new_bitmaps.clear();
            for (auto i = prime_set_bitmaps.begin(); i != prime_set_bitmaps.end(); i++) {
                for (; i->second < bitmaps.size(); i->second++) {
                    bool e = 0;
                    // Check bitmap for left or right set of primes
                    if (!((i->first[0] & bitmaps[i->second][0].first) || (i->first[1] & bitmaps[i->second][1].first))) {
                        break;
                    }
                }
                if (i->second == bitmaps.size()) {
                    // Reached the end for this path
                    done = true;
                    final_bitmaps.push_back(i->first);
                } else if (!done) {
                    for (int j = 0; j < 2; j++) {
                        for (auto k = bitmaps[i->second][j].second.begin(); k != bitmaps[i->second][j].second.end(); k++) {
                            //cout<<k<<" "<<*l<<endl;
                            if (!(bit_test(i->first[0], *k) || bit_test(i->first[1], *k))) {
                                array<int1024_t, 2>bitmap{ i->first[0], i->first[1] };
                                bit_set(bitmap[j], *k);
                                bool e = true;
                                if (done_second) {
                                    for (auto l = final_bitmaps.begin(); l != final_bitmaps.end(); l++) {
                                        if ((*l)[0] & bitmap[0] == (*l)[0] && (*l)[0] & bitmap[1] == (*l)[0]) {
                                            e = false;
                                            break;
                                        }
                                    }
                                }
                                if (e) {
                                    new_bitmaps.push_back(make_pair(bitmap, i->second + 1));
                                }
                            }
                        }
                    }
                }
            }

            sort(new_bitmaps.begin(), new_bitmaps.end());
            new_bitmaps.erase(unique(new_bitmaps.begin(), new_bitmaps.end()), new_bitmaps.end());
            prime_set_bitmaps = new_bitmaps;
            cerr << x << ": " << "prime set size: " << prime_set_bitmaps.size() << "\r";
        }
        cerr << "                                     \r";
        if (!done && prime_set_bitmaps.size() <= 40000000) {
            continue;
        }

        size_t w = primes_both_sides.size();

        cout << x << " -> " << final_bitmaps.size() << " " << w << " " << (final_bitmaps.size() * ((size_t)1 << w)) << endl;

        if (done && (final_bitmaps.size() * ((size_t)1 << w)) < ((size_t)1 << 33)) {

            vector<array<int1024_t, 2>>prime_sets(final_bitmaps.size());
            for (size_t i = 0; i < final_bitmaps.size(); i++) {
                array<int1024_t, 2> h = { 1,1 };
                for (int j = 0; j < 2; j++) {
                    int1024_t c = final_bitmaps[i][j];
                    // cout << "cur_index: " << cur_index << endl;
                    int k = 0;
                    while (c > 0) {
                        if (c % 2) {
                            h[j] *= rev_prime_bit_representations[k];
                        }
                        c >>= 1;
                        k++;
                    }
                }
                h[0] *= prod_unique_pfs_x;
                prime_sets[i] = h;
            }
            vector<thread> threads(min((size_t)1 << w, (size_t)num_threads));
            vector<promise<int1024_t>> responses(threads.size());
            for (size_t i = 0; i < threads.size(); i++) {
                threads[i] = thread(&process_subset, &prime_sets, &primes_both_sides, x, i, num_threads, &responses[i]);
            }

            int1024_t m = 0;
            for (size_t i = 0; i < threads.size(); i++) {
                threads[i].join();
                int1024_t o = responses[i].get_future().get();
                if (!m || o < m) {
                    m = o;
                }
            }

            double rt = (double)((chrono::duration<double>)(std::chrono::system_clock::now() - start)).count();
            total_time += rt;
            cout << x << " -> " << m << " : " << rt << " secs; total " << total_time << " secs" << endl;
        }
        else if (done) {
            double rt = (double)((chrono::duration<double>)(std::chrono::system_clock::now() - start)).count();
            total_time += rt;
            cout << x << " skipping because too many to check : " << rt << " secs; total " << total_time << " secs" << endl;
        }
        else {
            double rt = (double)((chrono::duration<double>)(std::chrono::system_clock::now() - start)).count();
            total_time += rt;
            cout << x << " skipping because search got too broad : " << rt << " secs; total " << total_time << " secs" << endl;
        }
    }
}