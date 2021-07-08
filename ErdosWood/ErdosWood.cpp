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
int S(set<int> &x) {
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

void M(vector<array<int1024_t, 2>> * q, set<int> * y, int x, size_t ct, int nt, promise<int1024_t> * pr) {
    int1024_t m = 0;
    size_t w = y->size();
    size_t s = (size_t)1 << w;
    size_t t = s * q->size();
    if (ct == 0 && t > 100000) {
        if (t > 10000000) {
            cerr << "00.0% done\r";
        }
        else {
            cerr << "00% done\r";
        }
    }
    for (size_t i = ct; i < s; i+=nt) {
        if (ct == 0 && t > 100000) {
            if (t > 10000000) {
                if ((i / nt) % max((size_t)1, s / nt / 1000) == 0) {
                    cerr << setfill('0') << setw(4) << string_format("%0.1f", ((double)100) * i / s) << "\r";
                }
            }
            else if ((i / nt) % max((size_t)1, s / nt / 100) == 0) {
                cerr << setfill('0') << setw(2) << 100 * i / s << "\r";
            }
        }
        array<int1024_t, 2>p = { 1,1 };
        auto k = y->begin();
        for (int j = 0; j < w; j++, k++) {
            p[bit_test(i, j)] *= *k;
        }
        //cout << x << " " << p[0] << " " << p[1] << endl;

        for (auto j = q->begin(); j != q->end(); j++) {
            int1024_t g0, g1, f0, f1;
            int64_t f2, f3;
            g0 = (*j)[0] * p[0]; g1 = (*j)[1] * p[1];
            //cout << g0 << " " << g1 << endl;
            vector<int1024_t>d;
            d.reserve(1000);
            vector<int64_t>e;
            e.reserve(1000);
            if (g0 > g1) { swap(g0, g1); }
            f0 = g0; f1 = g1;
            while (f0 > 1 && (f0 >= INT32_MAX / 2 || f1 >= INT32_MAX / 2)) {
                int1024_t n0,n1;
                divide_qr(f1, f0, n1, n0);
                d.push_back(n1);
                f1 = f0;
                f0 = n0;
            }
            if (f0 > 1) {
                f2 = (int64_t)f0;
                f3 = (int64_t)f1;
                while (f2 > 1) {
                    int64_t n2,n3;
                    divide_qr(f3, f2, n3, n2);
                    e.push_back(n3);
                    f3 = f2;
                    f2 = n2;
                }
            }
            f2 = 0; f3 = 1;
            for (auto k = e.rbegin(); k != e.rend(); k++) {
                int64_t n2 = f3;
                f3 = *k * f3 + f2;
                f2 = n2;
            }
            f0 = f2; f1 = f3;
            for (auto k = d.rbegin(); k != d.rend(); k++) {
                //cout<<f[0]<<" "<<f[1]<<" "<<(*j)<<endl;
                int1024_t n = f1;
                f1 = *k * f1 + f0;
                f0 = n;
            }
            int1024_t K = (x * f0 + g0 / 2) / g0;
            int1024_t o = min(abs(f1 * x - g1 * K) * g0, abs(f0 * x - g0 * K) * g1);
            //cout << x << " " << f0 << " " << f1 << " " << g0 << " " << g1 << " " << K << " " << o << endl;
            //cout<<o<<" m "<<m<<endl;
            if (!m || o < m) { m = o; }
        }
    }
        //cout<<endl;
    pr->set_value(m);
}

int main()
{
    int nt = 8;
    double tt = 0;
    for (int x = 631; x < 1000; x++) {
        auto start = chrono::system_clock::now();
        vector<array<set<int>, 2>>v(x - 1), nv;
        vector<pair<array<int1024_t, 2>, int>>r, s;
        vector<array<int1024_t, 2>>f;
        map<int, int>c;

        set<int>y;
        int1024_t t = 1;
        int mp = 0;
        
        for (int i = 2; i < x; i ++) {
            if (!v[i - 1][0].size()) {
                for (int j = 1; j < (x - 1) / i + 1; j++) {
                    v[i * j - 1][0].insert(i);
                    v[x - i * j - 1][1].insert(i);
                }
                if (x % i == 0) {
                    t *= i;
                }
                else if (!mp) {
                    mp = i;
                }
            }
        }
        if (v[0][1].size() == 1) {
            continue;
        }

        for (auto i = v.begin(); i != v.end(); i++) {
            bool e = 0;
            if ((*i)[0].size()) {
                for (auto j = (*i)[0].begin(); j != (*i)[0].end(); j++) {
                    if (t % *j == 0) {
                        e = 1;
                        break;
                    }
                }
            }
            if (e) { continue; }
            if (S((*i)[0]) != mp) {
                nv.push_back(*i);
            }
            for (int j = 0; j < 2; j++) {
                for (auto k = (*i)[j].begin(); k != (*i)[j].end(); k++) {
                    auto ci = c.find(*k);
                    if (ci == c.end()) {
                        c.insert(make_pair(*k, 1));
                    }
                    else {
                        ci->second++;
                    }
                    //cout << *k << " ";
                }
                //cout << "-";
            }
                //cout << endl;
        }
        v = nv;

        map<int, int>ns;
        vector<int>nr;
        vector<array<pair<int1024_t,vector<unsigned>>,2>>bs;
        int cn = 0;
        for (auto i = v.begin(); i != v.end(); i++) {
            if (((*i)[0].size() == 1 && (*i)[0].size() == 1 && c[*((*i)[0].begin())] < 3 && c[*((*i)[1].begin())] < 3)) {
                int z = *((*i)[0].begin());
                if (z != *((*i)[1].begin())) { z *= *((*i)[1].begin()); };
                if (!y.count(z)) {
                    y.insert(z);
                    //cout<<z<<endl;
                }
            }
            else {
                array<pair<int1024_t,vector<unsigned>>,2>b;
                for (int j = 0; j < 2; j++) {
                    b[j].first = 0;
                    for (auto k = (*i)[j].begin(); k != (*i)[j].end(); k++) {
                        auto ni = ns.find(*k);
                        unsigned c;
                        if (ni == ns.end()) {
                            ns.insert(make_pair(*k, cn));
                            nr.push_back(*k);
                            c = cn;
                            cn++;
                        }
                        else {
                            c = ni->second;
                        }
                        b[j].second.push_back(c);
                        b[j].first |= ((int1024_t)1) << c;
                    }
                }
                bs.push_back(b);
            }
        }

        sort(bs.begin(), bs.end(), [](array<pair<int1024_t, vector<unsigned>>, 2>&x, array<pair<int1024_t, vector<unsigned>>, 2>&y) {
            return min(x[0].first, x[1].first) < min(y[0].first, y[1].first);
        });
        //cout << "bs size: " << bs.size() << endl;

        //for (auto i = bs.begin(); i != bs.end(); i++) {
        //    for (auto j = 0; j < 2; j++) {
        //        int k = 0;
        //        int1024_t c = (*i)[j].first;
        //        while (c > 0) {
        //            if (c % 2) {
        //                cout << nr[k] << " ";
        //            }
        //            c >>= 1;
        //            k++;
        //        }
        //        cout << "-";
        //    }
        //    cout << endl;
        //}
        //cout << endl;

        //for (auto i = v.begin(); i != v.end(); i++) {
        //    for (auto j = 0; j < 2; j++) {
        //        for (auto k = (*i)[j].begin(); k != (*i)[j].end(); k++) {
        //            cout << *k << " ";
        //        }
        //        cout << " - ";
        //    }
        //    cout << endl;
        //}
        r.push_back(make_pair(array<int1024_t, 2>{((int1024_t)1) << ns[mp], 0}, 0));
        // cout << "v size: " << v.size();
        //int ii = 0;
        bool D = false, D2 = false;
        while (r.size() > 0 && r.size() < 40000000) { // memory issues above this
            //cout << "ii: " << ii << endl;
            //for (auto j = 0; j < 2; j++) {
            //    for (auto k = (*i)[j].begin(); k != (*i)[j].end(); k++) {
            //        cout << *k << " ";
            //    }
            //    cout << " - ";
            //}
            //cout << endl;
            if (D && !D2) {
                D2 = true;
            }
            s.clear();
            for (auto i = r.begin(); i != r.end(); i++) {
            //    for (auto j = 0; j < 2; j++) {
            //        int k = 0;
            //        int1024_t c = (*i).first[j];
            //        while (c > 0) {
            //            if (c % 2) {
            //                cout << nr[k] << " ";
            //            }
            //            c >>= 1;
            //            k++;
            //        }
            //        cout << "-";
            //    }
            //    cout << endl;
                for (; i->second < bs.size(); i->second++) {
                    bool e = 0;
                    // Check bitmap for left or right set of primes
                    if (!((i->first[0] & bs[i->second][0].first) || (i->first[1] & bs[i->second][1].first))) {
                        break;
                    }
                }
                if (i->second == bs.size()) {
                    // Reached the end for this path
                    D = true;
                    f.push_back(i->first);
                } else if (!D) {
                    for (int j = 0; j < 2; j++) {
                        for (auto k = bs[i->second][j].second.begin(); k != bs[i->second][j].second.end(); k++) {
                            //cout<<k<<" "<<*l<<endl;
                            if (!(bit_test(i->first[0], *k) || bit_test(i->first[1], *k))) {
                                array<int1024_t, 2>b{ i->first[0], i->first[1] };
                                bit_set(b[j], *k);
                                bool e = true;
                                if (D2) {
                                    for (auto l = f.begin(); l != f.end(); l++) {
                                        if ((*l)[0] & b[0] == (*l)[0] && (*l)[0] & b[1] == (*l)[0]) {
                                            e = false;
                                            break;
                                        }
                                    }
                                }
                                if (e) {
                                    s.push_back(make_pair(b, i->second + 1));
                                }
                            }
                        }
                    }
                }
                //cout << endl;
            }

            //cout<<"s size: "<<s.size()<<endl;

            sort(s.begin(), s.end());
            s.erase(unique(s.begin(), s.end()), s.end());
            r = s;
            cerr << x << ": " << "r size: " << r.size() << "\r";
            //map<size_t, int>rs;
            //for (auto j = r.begin(); j != r.end(); j++) {
            //    size_t js = j->first[0].size() + j->first[1].size();
            //    auto k = rs.find(js);
            //    if (k == rs.end()) {
            //        rs.insert(make_pair(js, 1));
            //    }
            //    else {
            //        k->second++;
            //    }
            //}
            //for (auto k = rs.begin(); k != rs.end(); k++) {
            //    cout << k->first << ": " << k->second << endl;
            //}
        }
        cerr << "                     \r";
        if (!D && r.size() <= 40000000) {
            continue;
        }

        //size_t ms = 0;
        //for (auto i = r.begin(); i != r.end(); i++) {
        //    size_t cs = i->first[0].size() + i->first[1].size();
        //    if (cs < ms || !ms) { ms = cs; }
        //}
        size_t w = y.size();

        cout << x << " -> " << f.size() << " " << w << " " << (f.size() * ((size_t)1 << w)) << endl;

        if (D && (f.size() * ((size_t)1 << w)) < ((size_t)1 << 33)) {

            //for (auto j = t.begin(); j != t.end(); j++) {
            //    cout << "t:" << *j << endl;
            //}

            vector<array<int1024_t, 2>>q(f.size());
            for (size_t i = 0; i < f.size(); i++) {
                array<int1024_t, 2> h = { 1,1 };
                for (int j = 0; j < 2; j++) {
                    int1024_t c = f[i][j];
                    // cout << "c: " << c << endl;
                    int k = 0;
                    while (c > 0) {
                        if (c % 2) {
                            h[j] *= nr[k];
                        }
                        //cout << k << " " << c << " " << (*nr)[k] << " " << c % 2 << " " << h[j] << endl;
                        c >>= 1;
                        k++;
                    }
                    //cout << "h: " << h[j] << endl;
                    // cout<< "-";
                }
                h[0] *= t;
                q[i] = h;
            }
            vector<thread> ts(min((size_t)1 << w, (size_t)nt));
            vector<promise<int1024_t>> ps(ts.size());
            for (size_t i = 0; i < ts.size(); i++) {
                ts[i] = thread(&M, &q, &y, x, i, nt, &ps[i]);
            }

            int1024_t m = 0;
            for (size_t i = 0; i < ts.size(); i++) {
                ts[i].join();
                int1024_t o = ps[i].get_future().get();
                if (!m || o < m) {
                    m = o;
                }
            }

            double rt = (double)((chrono::duration<double>)(std::chrono::system_clock::now() - start)).count();
            tt += rt;
            cout << x << " -> " << m << " : " << rt << " secs; total " << tt << " secs" << endl;
        }
        else if (D) {
            double rt = (double)((chrono::duration<double>)(std::chrono::system_clock::now() - start)).count();
            tt += rt;
            cout << x << " skipping because too many to check : " << rt << " secs; total " << tt << " secs" << endl;
        }
        else {
            double rt = (double)((chrono::duration<double>)(std::chrono::system_clock::now() - start)).count();
            tt += rt;
            cout << x << " skipping because search got too broad : " << rt << " secs; total " << tt << " secs" << endl;
        }
    }
}