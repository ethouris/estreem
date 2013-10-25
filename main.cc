#include <iostream>
#include <sstream>
#include <algorithm>
#include <iterator>
#include <string>
#include <vector>
#include <list>
#include <tuple>
#include <set>
#include <stdexcept>
#include <csignal>
#include <cctype>
#undef _S // fuckin cctype defines and exports _S
#include <climits>

//*
inline std::string operator"" _S (const char* chars, size_t size)
{
    return std::string( chars, size );
}
// */

// GENERAL PURPOSE UTILITY
template<typename T, typename _Alloc = std::allocator<T> >
class rvector: public std::vector<T, _Alloc>
{
	typedef std::vector<T> base_t;
public:
	// Standard stuff
	using typename base_t::value_type;
	using typename base_t::pointer;
	using typename base_t::const_pointer;
	using typename base_t::reference;
	using typename base_t::const_reference;
	using typename base_t::size_type;
	using typename base_t::difference_type;
	using typename base_t::allocator_type;

	using base_t::size;
	using base_t::max_size;
	using base_t::resize;

	using std::vector<T, _Alloc>::vector;

	// Make the reverse iterator the main iterator
	typedef typename base_t::reverse_iterator iterator;
	typedef typename base_t::const_reverse_iterator const_iterator;
	typedef typename base_t::iterator reverse_iterator;
	typedef typename base_t::const_iterator const_reverse_iterator;

	iterator begin() { return base_t::rbegin(); }
	const_iterator begin() const { return base_t::rbegin(); }
	const_iterator cbegin() const { return base_t::rbegin(); }
	reverse_iterator rbegin() { return base_t::begin(); }
	const_reverse_iterator rbegin() const { return base_t::begin(); }
	const_reverse_iterator crbegin() const { return base_t::begin(); }

	iterator end() { return base_t::rend(); }
	const_iterator end() const { return base_t::rend(); }
	const_iterator cend() const { return base_t::rend(); }
	reverse_iterator rend() { return base_t::end(); }
	const_reverse_iterator rend() const { return base_t::end(); }
	const_reverse_iterator crend() const { return base_t::end(); }

	reference operator[] (size_t ix) { return base_t::operator[](size()-1-ix); }
	const_reference operator[](size_t ix) const { return base_t::operator[](size()-1-ix); }
	reference at (size_t ix) { return base_t::at(size()-1-ix); }
	const_reference at(size_t ix) const { return base_t::at(size()-1-ix); }

	// The body is copied from std::vector, however it means something
	// different due to different definition of begin() and end().
	reference front() { return *begin(); }
	const_reference front() const { return *begin(); }
	reference back() { return *(end() - 1); }
	const_reference back() const { return *(end() - 1); }

	// data() is something you should never use because the elements
	// read from it will be in reverse order.
	pointer data() { return &((*this)[0]); }
	const_pointer data() const { return &((*this)[0]); }

	// No operations on the back
	void push_back(const value_type& __x) = delete;
	void push_back(value_type&& __x) = delete;
	void pop_back() = delete;

	// Operate on the front
	void push_front(const value_type& x) { return base_t::push_back(x); }
	void push_front(value_type&& x) { return base_t::push_back(x); }
	void pop_front() { return base_t::pop_back(); }
};



using namespace std;

// Log utilities
template <typename Type> inline Type make_printable(const Type& t ) { return t; }
inline string make_printable(const char* x) { return string(x); }
inline string make_printable(const vector<size_t>& ixt)
{
    ostringstream os;
    os << "{ ";
    for (size_t i: ixt)
    {
        os << i << ", ";
    }
    os << "}";
    return os.str();
}
inline string make_printable(const vector<char>& s)
{
	string out;
	for (char c: s)
	{
		out += c;
		out += ' ';
	}

	return out;
}
inline string make_printable(const set<char>& in) { string out; copy(in.begin(), in.end(), back_inserter(out)); return out; }

template<typename Stream>
inline Stream& logs1(Stream& orr) { return orr; }

template <typename Stream, typename Arg, typename... Args>
inline Stream& logs1(Stream& orr, const Arg& i, const Args&... args)
{ orr << make_printable( i ); return logs1( orr, args... ); }

template<typename... Args> inline
void log_imp( Args&&... args )
{
    cerr << "ES> ";
    logs1( cerr, args... );
    cerr << endl;
}

struct Logger
{
    int level = 0;

    template <class... Args>
    void operator()(Args&&... args) { ::log_imp( string( 3*level, ' ' ), args... ); }
} eslog;


const char whitespace [] = " \t\n";
const bool empty_whitespace = true;

class LexemeBase;			// root lexeme class
class DirectLexeme;			// lexeme assigned 1to1 to a character
class CharLexeme;			// an alternative or range of characters, possibly with flags
class AlternativeLexeme;	// an alternative of arbitrary lexemes
class SequenceLexeme;		// a lexeme that contains a sequence of arbitrary lexemes
class Lexeme;				// value wrapper class

const LexemeBase* keyword_lexeme = 0;

// enum class Occurrence {

enum class Occurrence {
    exact = 0,
    inverted = 1,
    optional = 2,
    variadic = 4
};

// bitwise operator
Occurrence operator|(Occurrence a, Occurrence b) { return Occurrence( int(a) | int(b) ); }
Occurrence operator&(Occurrence a, Occurrence b) { return Occurrence( int(a) & int(b) ); }

bool is( Occurrence value, Occurrence mask )
{
    return (value & mask) == mask;
}


ostream& operator<<(ostream& os, Occurrence oxx)
{
    if ( oxx == Occurrence::exact )
        return os << "exact";

    bool any = false;
    if ( is(oxx, Occurrence::inverted) )
    {
        os << "inverted";
        any = true;
    }

    if ( is(oxx, Occurrence::optional) )
    {
        if ( any )
            os << "|";

        os << "optional";
        any = true;
    }

    if ( is(oxx, Occurrence::variadic) )
    {
        if ( any )
            os << "|";

        os << "variadic";
    }

    return os;
}

ostream& operator<<(ostream& sout, const Lexeme& lx);


bool operator >=(Occurrence val, Occurrence mask)
{
    return is(val, mask);
}
bool operator <=(Occurrence val, Occurrence mask) { return is(mask,val); }
bool operator <(Occurrence val, Occurrence mask) { return int(mask & val) == 0; }
bool operator >(Occurrence val, Occurrence mask) { return mask < val; }

string flaggen( string name, Occurrence oxx, bool trailing = false )
{
    if (  oxx >=  Occurrence::inverted  )
        name = "^" + name;
    else if ( trailing )
        name = "|" + name;

    if (  oxx >=  Occurrence::variadic  )
        name += "...";

    if (  oxx >=  Occurrence::optional  )
        name += "?";
    return name;
}

// }

string flaggen( Lexeme d );


struct TupleRangeIt
{
    char cp;
    char operator*() { return cp; }
    TupleRangeIt& operator++() { ++cp; return *this; }
    TupleRangeIt operator++(int) { TupleRangeIt that = *this; ++cp; return that; }
    bool operator != (const TupleRangeIt& oth) { return oth.cp != cp; }
};

namespace std
{
inline TupleRangeIt begin(tuple<char,char>& t) { TupleRangeIt r; r.cp = get<0>(t); return r; }
inline TupleRangeIt end(tuple<char,char>& t) { TupleRangeIt r; r.cp = get<1>(t)+1; return r; }
}

// I know that constructor works, but the order of class definition
// causes that I cannot use it when the code is inside the LexemeBase-derived class.
const Lexeme& Lexeme_wrap(LexemeBase*);

class LexemeBase
{
    friend class Lexeme;
protected:
    static DirectLexeme* globmatch[256];

public: // XXX consider protecting these fields

    // tags for constructor
    static constexpr struct range_t {} range {};
    static constexpr struct alt_t {} alt {};
    static constexpr struct sequence_t {} seq {};
    static constexpr struct string_t {} str {};

    struct error: public std::exception
    {
        string detail;
        error(const string& det): detail(det) {}
        const char* what() const noexcept override { return detail.c_str(); }
    };

	enum class Match
	{
		failed, // The incrementation succeeded and the current character did not match this lexeme.
		found, // The incrementation succeeded (if was requested) and the character matches
		full, // The incrementation failed (not possible to be a result if incrementation was not requested)
	};

    string lexname_;

    // Bottom-up bindings - seem little necessary
    /// Those that use this one as one of possibilities (|)
    vector<LexemeBase*> extenders_;
    /// Those that use a sequence of tokens and *this is the first of them.
    /// Note that they have to define here also what occurrence this one has.
    vector<LexemeBase*> followers_;

	// Top-down bindings - required for checks for expected
	vector<Lexeme> extends_;

public:
    string name() const { return lexname_; }
    typedef rvector<size_t> index_t;

    LexemeBase( const string& n ): lexname_(n) {}

	// Instruction for the implementation:
	// if 'current' contains any value at position 0, it means that
	// this is the value that has been already once reported as matching.
	// Otherwise there's no "previous" match for this lexeme yet.
	// The underlying implementation should do then the following:
	// - if there is current[0]
	//     - depending on the levelness of the lexeme
	//        - simple lexemes do direct character comparison
	//			 - if variadic flag found, compare with the current
	//			 	 - if matches (modulo inverted flag!), return Match::found with index unchanged
	//			 - otherwise return Match::full
	//        - complex lexemes do sub-lexemes match calls
	//           - NOTE: called with the index already set, so it satisfies current[0] condition
	//           - when Match::failed, it means that the sub-lexeme succeeded with incrementation,
	//             but failed with matching. Consider current match is failed, so return Match::failed
	//           - when Match::found, return Match::found. When full==true, check fullness accordingly at your side!
	//			     - NOTE: in this case, index has been already updated to the new match!
	//           - when Match::full: ((most expected!))
	//               - when variadic flag is set, try to repeat the same sub-lexeme from the beginning (empty index)
	//				 - (NOTE: the only complex lexeme that has variadic flag is RichLexeme)
	//               - otherwise, try to do incrementation at your level; if not possible, return Match::full
	//               - if incrementation succeeded, call the sub-lexeme match with empty index
	//               - (NOTE: only SequenceLexeme is capable of doing self-incrementation)
	//               - act according to the result of the submatch and possibly flags (optional, inverted)
	// - otherwise (if there is an empty index - means this lexeme at the current position wasn't tried yet)
	//     - depending on the levelness of the lexeme
	//		  - simple lexemes do direct character comparison
	//           - if the character matches (modulo 'inverted' flag), return Match::found (index: TRYIX)
	//           - otherwise:
	//             - if 'optional' flag, try to increment and try again (TRYIX++) (return Match::full if not possible)
	//             - otherwise return Match::failed
	//           - depending on whether the character matches, result is Match::found or Match::failed
	//           - simple lexemes are never sequence lexemes, so full = true always
	//	      - complex lexemes do sub-lexemes match calls
	//           - NOTE: this time called with empty index, so possible are only Match::found and Match::failed
	//           - invert the sub-result, if 'inverted' flag found
	//           - set TRYIX = 0 (only those, which can increment itself)
	//           - when Match::failed, it means that no incrementation was done (empty index)
	//			   and no match found, so:
	//             - if 'optional' flag, do incrementation at your level (return Match::full if incrementation not possible)
	//               and try again (TRYIX++)
	//             - if no special flags - return Match::failed
	//           - when Match::found - attach the [TRYIX] to the index and return Match::found
    virtual Match match( char ch, index_t& current, bool& full ) const = 0;
    virtual set<char> characters() const = 0;
	bool extends(Lexeme l) const;

    // Those below have default implementation; specific definition
    // is expected in the classes that define a lexeme of a single character.
    virtual bool single() const { return false; }
    virtual Occurrence flags() const { return Occurrence::exact; }

    // debugging stuff
    string show_structure() const
    {
        ostringstream out;
        out << name();
        if ( !extenders_.empty() )
        {
            out << " AKA (";
            for (auto i: extenders_)
            {
                out << i->name() << "=" << flaggen(name(), i->flags()) << " ";
            }
            out << ")";
        }

        if ( !followers_.empty() )
        {
            out << " FLW ( ";
            transform(followers_.begin(), followers_.end(), ostream_iterator<string>(out, " "), [](const LexemeBase* in) {
                return in->name();
            });
            out << ")";
        }
        else
        {
            out << " .";
        }

        return out.str();
    }

};

class EmptyLexeme: public LexemeBase
{
public:
	EmptyLexeme( string n ): LexemeBase( n ) {}

	virtual set<char> characters() const override { return set<char> {}; }
	virtual Match match(char ch, index_t&, bool& full) const override
	{
		full = true;
		return Match::full;
	}
};

class DirectLexeme: public LexemeBase
{
    char target_;
public:
    DirectLexeme( string n, char c ): LexemeBase(n), target_(c)
    {
    }

    virtual set<char> characters() const override
    {
        return set<char> { target_ };
    }

    virtual Match match( char ch, index_t& current, bool& full ) const override
	{
		eslog( "match/direct: '", ch, "' against '", target_, "' position: ", current );
		full = true;

		if ( !current.empty() )
		{
			return Match::full;
		}

		// Comes with a new match, so do the first match.
		if ( ch == target_ )
		{
			current.push_front(0);
			return Match::found;
		}

		return Match::failed;
	}

    virtual bool single() { return true; }

};


// This is a derivative lexeme, so it must link
// itself to direct lexemes. Additionally it may have flags.
//
// lexeme plus = "+"  // direct lexeme
// lexeme cipher = '09' // char lexeme with no flags and 0 1 2 ... 9 characters
// lexeme anyplus = plus...? cipher... // SequnceLexeme out of 1 CharLexeme "plus...?" and "cipher..."
// Artificially created:
// - gen:plusVarOpt = CharLexeme made of "plus" and added with flags
// - gen:cipherVar = CharLexeme made of "cipher" and added with flags
// So, lexeme anyplus = gen:plusVarOpt gen:cipherVar (SequenceLexeme with these two sequenced)

class CharLexeme: public LexemeBase
{

protected:

    vector<tuple<char,char>> ranges_;
    set<char> alternatives_;
    Occurrence occurrence_ = Occurrence::exact;

    static string as_ranges( const vector<tuple<char,char>>& in )
    {
        string out;
        for(auto i: in)
        {
            out += get<0>(i);
            out += "-";
            out += get<1>(i);
        }
        return out;
    }

    static string as_alts( const set<char>& in ) { string out; copy(in.begin(), in.end(), back_inserter(out) ); return out; }
public:

    CharLexeme(string n, Occurrence oxx = Occurrence::exact): LexemeBase(n), occurrence_(oxx) {}

    template <class... Args>
    CharLexeme( string n, Occurrence oxx, Args... args ): LexemeBase(n), occurrence_(oxx)
    {
        init(args...);
    }

	void update_alternatives(const set<char>& alt)
	{
		for (auto a: alt)
		{
			eslog( " ... ", name(), ": adding alternative character: ", a );
			extends_.push_back(Lexeme_wrap(globmatch[int(a)]));
		}
	}

	void update_ranges(const vector<tuple<char,char>>& ranges)
	{
		for (auto r: ranges)
		{
			char from, to;
			tie(from, to) = r;

			eslog( " ... ", name(), ": adding range of characters ", from, "-", to );
			if ( from > to )
				throw LexemeBase::error( "Init: " + name() + " range invalid: '" + from + to + "'!" );
			for (int i = from; i <= to; ++i )
				extends_.push_back(Lexeme_wrap(globmatch[i]));
		}
	}

    void import(const CharLexeme* other)
    {
        if ( other->occurrence_ != Occurrence::exact )
            throw LexemeBase::error( "Init: " + name() + ": source lexeme '" + other->name() + "' must have no flags!" );

        if ( !other->alternatives_.empty() )
		{
            alternatives_.insert(other->alternatives_.begin(), other->alternatives_.end());
			update_alternatives(other->alternatives_);
		}
        if ( !other->ranges_.empty() )
		{
            copy(other->ranges_.begin(), other->ranges_.end(), back_inserter(ranges_));
			update_ranges(other->ranges_);
		}
    }

    void import(const LexemeBase* other)
    {
        // Do special action on CharLexeme, as it should be treated differently
        auto cl = dynamic_cast<const CharLexeme*>(other);
        if ( cl )
            return import(cl);

        set<char> other_alternatives = other->characters();
        if ( !other_alternatives.empty() )
		{
            alternatives_.insert(other_alternatives.begin(), other_alternatives.end());
			update_alternatives(other_alternatives);
		}
    }

    void init() {}

    template <class... Args>
    void init( LexemeBase::range_t, char c1, char c2, Args... args )
    {
        init_range(c1, c2);
        init(args...);
    }

    template <class... Args>
    void init( LexemeBase::alt_t, char c, Args... args )
    {
        init_alt(c);
        init(args...);
    }

    void init_alt() {}
    void init_range() {}

    template <class... Args>
    void init_alt( char ch, Args... args )
    {
        LexemeBase* xtd = globmatch[int(ch)];
		string sym (1, ch);
        if ( !xtd )
        {
			if ( ch == '&' or ch == ':' or ch <= 32 or ch >= 127 )
			{
				ostringstream v;
				v << "&" << int(ch);
				sym = v.str();
			}
            DirectLexeme* dir = new DirectLexeme( "@char:"_S + sym, ch );
            globmatch[int(ch)] = dir;
            xtd = dir;
        }
		else
		{
			sym = xtd->name();
		}
		eslog( " ... lexeme '", name(), "' extends character '", sym, "'" );

        xtd->extenders_.push_back( this );
        alternatives_.insert(ch);
		extends_.push_back(Lexeme_wrap(xtd));

        // tailchain
        init_alt(args...);
    }

    template <class... Args>
    void init_range( char c1, char c2, Args... args )
    {
        for (char c = c1; c != c2; c++)
        {
            LexemeBase* xtd = globmatch[int(c)];
            if ( !xtd )
            {
                DirectLexeme* dir = new DirectLexeme( "@char:"_S + c, c );
                globmatch[int(c)] = dir;
                xtd = dir;
            }

            xtd->extenders_.push_back( this );
			extends_.push_back(Lexeme_wrap(xtd));
        }
		eslog( " ... ", name(), ": extends characters '", c1, "'-'", c2, "'" );
		ranges_.emplace_back(c1, c2);

        // tailchain
        init_range(args...);
    }

    virtual Match match( char ch, index_t& current, bool& full ) const override
    {
        string id = "[" + as_ranges(ranges_) + as_alts(alternatives_) + "]";
        eslog( "match/char: ", ch, " against ", name(), " = ", flaggen(id, occurrence_), " @ position ", current );

		if ( !current.empty() )
		{
			eslog( " -- as re-match:" );
			if ( occurrence_ >= Occurrence::variadic )
			{
				if ( found_in_set(ch) != is(occurrence_, Occurrence::inverted) )
				{
					eslog( " -- matching at the same position due to variadic flag" );
					full = true;
					return Match::found;
				}
				// Otherwise, forget it and fallback to incremented match
			}
			// No catch for variadic, so request increment 
			// But well, CharLexeme cannot increment! So...
			eslog( " -- requesting up incrementation" );
			return Match::full;
		}

		eslog( " -- as new match" );

		if ( found_in_set(ch) != is(occurrence_, Occurrence::inverted) )
		{
			current.push_front(0);
			full = true;
			eslog( " -- matches new position" );
			return Match::found;
		}
		
		if ( is(occurrence_, Occurrence::optional) )
		{
			eslog( " -- no match, but optional - requesting up incrementation" );
			// Incrementation not possible, so
			return Match::full;
		}

		eslog( " -- no match and required - result is no match" );
		return Match::failed;
    }

    virtual bool single() const override { return true; }
    virtual Occurrence flags() const override { return occurrence_; }
    virtual set<char> characters() const override
    {
        set<char> out = alternatives_;
        for (auto range: ranges_)
        {
            for (char c: range)
            {
                out.insert(c);
            }
        }

        return out;
    }

	// match utility
	bool found_in_set(char ch) const
	{
		int c = alternatives_.count( ch );
		if ( c != 0 )
		{
			eslog( "-- found in alternatives" );
			return true;
		}

		eslog( " --- not in alternatives, checking in ranges" );
		auto ff = find_if(ranges_.begin(), ranges_.end(), [ch](tuple<char, char> ct) {
				char left, right;
				tie(left, right) = ct;
				eslog( "Checking in range (", left, " ", ch, " ", right, ")" );
				return left <= ch and ch <= right;
				});
		if ( ff != ranges_.end() )
		{
			eslog( "-- found in ranges" );
			return true;
		}

		eslog( "-- not found" );
		return false;
	}

	// Logging/debug purpose
    string rev_def() const
    {
        string out = "'";
        for (auto i: ranges_)
        {
            out += get<0>(i);
            out += get<1>(i);
        }

        if ( !alternatives_.empty() )
        {
            if ( alternatives_.size() == 1 )
            {
                out += *alternatives_.begin();
                out += "'";
            }
            else
            {
                out += "'|/";
                for (auto i: alternatives_)
                    out += i;
                out += "/";
            }
        }
        else
        {
            out += "'";
        }

        return out;
    }

};

// This is only an intermediate Lexeme type that points an underlying
// lexeme of any kind and applies additional flags.
class RichLexeme: public LexemeBase
{
    LexemeBase* sub_;
	Occurrence occurrence_ = Occurrence::exact;

public:
    RichLexeme( string n, LexemeBase* sub, Occurrence flags ): LexemeBase(n), sub_(sub), occurrence_(flags)
    {
		eslog( " ... ", n, " is a rich lexeme of ", sub->name(), " with flags ", flags);
		// XXX Hey, not so fast! Remember to link those with
		// inverted flag properly!
        sub->extenders_.push_back( this );
		extends_.push_back(Lexeme_wrap(sub));
    }

    virtual Match match( char ch, index_t& current, bool& full ) const override
    {
		size_t current0 = 0;
        eslog( "match/rich: ", ch, " for ", name() );

		if ( !current.empty() )
		{
			current0 = current[0];
			current.pop_front();
			// derive the full flag - RichLexeme doesn't mind here.
			Match submatch = sub_->match(ch, current, full);
			if ( submatch == Match::full )
			{
				if ( occurrence_ >= Occurrence::variadic )
				{
					// Start the match anew.
					current = index_t {};
					submatch = sub_->match(ch, current, full);
					// Matching at none position, 'full' cannot be a result!
					if ( (submatch == Match::found) != (occurrence_ >= Occurrence::inverted) )
					{
						// Note that 'current' is already updated, so return the status quo.
						current.push_front(current0+1); // current0+1 to distinguish between 1st time and next times
						return Match::found;
					}
				}

				// Ok, so now we have the increment request, with no retrying.
				// RichLexeme as itself cannot increment, so forward the full flag.
				current.push_front(current0);
				return Match::full;
			}

			// Restore it.
			current.push_front(current0);

			// We have the sub-index already incremented. Result is either match or no match.
			bool submatch_match = (submatch == Match::found) != (occurrence_ >= Occurrence::inverted);
			// Now submatch_match is the real value of whether "we have a match", regarding inverted flag.

			if ( submatch_match )
			{
				// So, now we have the match found and our index is properly incremented.
				// Now just push the current value on the stack and return match.
				return Match::found;
			}

			// Last chance. It doesn't match, but maybe it was optional?
			// Or, it has been already checked on that position?
			if ( occurrence_ >= Occurrence::optional || current0 > 0 )
			{
				// Say, "I would have to increment to know for sure"
				return Match::full;
			}

			return Match::failed;
		}


		Match submatch = sub_->match(ch, current, full);
		bool submatch_match = (submatch == Match::found) != (occurrence_ >= Occurrence::inverted);
		// This is the first time, so push 0 on index.
		// This lexeme type is not incrementable, so there will be no other possibility.
		// Index value means whether already any variadic element was tried or not.
		current.push_front(0);

		if ( submatch_match )
		{
			return Match::found;
		}

		// Last chance. It doesn't match, but maybe it was optional?
		if ( occurrence_ >= Occurrence::optional )
		{
			return Match::full;
		}

		return Match::failed;
    }

    virtual set<char> characters() const override
    {
		return sub_->characters();
    }

	virtual Occurrence flags() { return occurrence_; }

};


// This is much like CharLexeme, but while CharLexeme can
// only link to DirectLexeme characters (and therefore it
// contains character defs directly, not pointers to DirectLexeme),
// this one can contain lexemes of any kind, including SequenceLexeme.
// AlternativeLexeme does not support flags; use RichLexeme to apply
// additionally any flags.
class AlternativeLexeme: public LexemeBase
{
    vector<LexemeBase*> alternatives_;

public:
    template <class... Args>
    AlternativeLexeme( string n, Args... args ): LexemeBase(n)
    {
        init(args...);
    }

    template <class... Args>
    void init( LexemeBase* alt, Args... args )
    {
		eslog( " ... ", name(), ": adding alternative: ", alt->name() );
        alternatives_.push_back(alt);
        alt->extenders_.push_back( this );
		extends_.push_back(Lexeme_wrap(alt));
        init(args...);
    }

    void init() {}

    virtual Match match( char ch, index_t& current, bool& full ) const override
    {
        eslog( "match/alt: ", ch, " for ", name() );

		// When there is anything on specified position, that is,
		// we have a position that is already confirmed, the number
		// on the 0th position means the number of alternative in
		// this lexeme. Because of that, if we have a confirmed
		// overflow from the underlying lexeme, we don't increment
		// the index at current position, but instead we generate
		// overflow for the caller.

		// So, first let's check the "already confirmed" situation...
		if ( !current.empty() )
		{
			size_t current0 = current[0];
			// Note: the index for this lexeme means which alternative
			// has been already matched.
			current.pop_front();

			// It means that *this must continue with THE SAME alternative,
			// as has been already found. The check is to state if with the
			// next character, this thing is still matching.
			// Note: full flag derived (as it also cannot increment itself)
			Match submatch = alternatives_[current0]->match(ch, current, full);
			current.push_front(current0);

			switch ( submatch )
			{
			case Match::full:
				// No flags, so no variadic branch.
				// No possibility to increment.
				return Match::full;

			case Match::failed:
				return Match::failed;

			case Match::found:
				return Match::found; // 'full' flag is derived and already set
			}
		}

		// ? current.pop_front();

		// Nothing matched yet, so find the first matching among these:
		for (size_t i = 0; i < alternatives_.size(); ++i)
		{
			// Now 'current' is empty; expect this call to fill it in.
			Match submatch = alternatives_[i]->match(ch, current, full);
			// It cannot be full, unless underneath you get an optional lexeme
			// Optional lexeme cannot be alternative; just treat it as no match.
			if ( submatch == Match::found )
			{
				current.push_front(i);
				full = true;
				return Match::found;
			}
			current.clear(); // false alert
		}

		// None matches? So, sorry...
		// 0 is same bad as the others, but it must be a valid index in alternatives_
		// for a case when the superlexeme is rich lexeme with inverted flag.
		current.push_front(0);
		return Match::failed;
    }

    virtual set<char> characters() const override
    {
        set<char> out;
        for (auto l: alternatives_)
        {
            auto st = l->characters();
            out.insert(st.begin(), st.end());
        }
        return out;
    }

    string rev_def() const
	{
		string out;
		for (auto& alt: alternatives_)
		{
			out += "|";
			out += alt->name();
		}

		return out.substr(1);
	}
};


class SequenceLexeme: public LexemeBase
{
    vector<LexemeBase*> sequence_;
    bool is_string_ = true;
public:
    template <class... Args>
    SequenceLexeme( string n, LexemeBase* first, Args... args): LexemeBase(n)
    {
        sequence_.push_back(first);
        first->followers_.push_back(this);
		extends_.push_back(Lexeme_wrap(first));

		eslog( " ... ", n, ": adding to sequence: ", first->name() );

        init(args...);
    }

    void init() {}

    template <class... Args>
    void init(LexemeBase* e, Args... args)
    {
        sequence_.push_back(e);
		eslog( " ... ", name(), ": adding to sequence: ", e->name() );
        init(args...);
    }

    void string_( bool nv ) { is_string_ = nv; }
    bool string_() const { return is_string_; }

    virtual Match match( char ch, index_t& current, bool& full ) const override
    {
		// XXX This procedure is valid only for sequence lexemes of type string.
		// Required additionally:
		// - comparison for lexemes of type sequence (before matching particular
		//   element of the sequence, but the first, the whitespace lexeme must
		//   be first matched)
		// - this should work correctly for both required and optional whitespace
		if ( !current.empty() )
		{
			size_t current0 = current[0];
			current.pop_front();

			bool sub_full = false;
			Match submatch = sequence_[current0]->match( ch, current, sub_full );
			while ( submatch == Match::full )
			{
				// Ok, now seriously try incrementation.
				++current0;
				if ( current0 >= sequence_.size() )
				{
					// Not possible, we've reached the end.
					return Match::full;
				}
				// So, engage new matching.
				current.clear();
				submatch = sequence_[current0]->match( ch, current, sub_full );
				current.push_front(current0);
				if ( submatch == Match::found )
					return Match::found;
				if ( submatch == Match::failed )
					return Match::failed;
				// Returning full from a lexeme that is tried first time
				// means something like the sub-lexeme has optional flag.
				// In this case just ignore it and take on the next one.
				// It may happen also that it get until the end and return full.
			}

			current.push_front(current0);

			if ( submatch == Match::failed )
				return Match::failed;

			if ( sub_full && current0+1 == sequence_.size() )
				full = true;

			return Match::found;
		}

		// Do the first match at the first item.
		// Possibly, however, skip lexemes which return full (these are optional)
		for (size_t current0 = 0; current0 < sequence_.size(); ++current0)
		{
			bool sub_full = false;
			Match submatch = sequence_[current0]->match(ch, current, sub_full);
			if ( submatch == Match::full )
				continue;
			current.push_front(current0);
			if ( submatch == Match::found )
			{
				if (sub_full && current0+1 == sequence_.size())
					full = true;
				return Match::found;
			}

			return Match::failed;
		}

		// Exit from the loop means that all parts were optional and have been skipped.
		return Match::full;

		/*
		// OLD IMP:

		// Cases to research:
		// - current == {} - then check the first one. Mind optional.
		// - current == { 1 } and
		//   - sequence_[1] is variadic

        // The index must contain at least 2 elements
        while ( current.size() < 2 )
        {
            eslog( " --- drilling down" );
            current.push_back(0); // extend the depth
        }
        eslog( " --- depth: ", current.size() );

        index_t r;
        if ( is_string )
        {
            eslog( "match/string: '", ch, "' against ", name(), " @ position ", current );
            r = match_string( ch, current );
        }
        else
        {
            eslog( "match/sequence: '", ch, "' against ", name(), " @ position ", current );
            r = match_sequence(ch, current);
        }
        return r;
		*/
    }

	/*

	index_t match_variadic( char ch, index_t current ) const
	{
		// This is a fallback in case when match at
		// exactly current didn't happen. If variadic
		// also doesn't provide a match, return {}.
        auto& here = current[0];

		// First, check if the previous element exists
		// and has variadic flag.
		if ( here == 0 || sequence[here-1]->flags() <= Occurrence::variadic )
			return index_t {};

		eslog( " --- Not matching @ that position, but trying previous variadic" );

		// Next, normally check the match for subindex.
		current[0]--;
		return match_string(ch, current, false);
	}

    index_t match_string( char ch, index_t current, bool check_variadic = true ) const
    {
        // XXX index_t operates strongly with its first element.
        // vector is not efficient for this use; consider using
        // deque or define some container that can share the following
        // elements with another "vector".
        index_t subindex;
        copy(current.begin()+1, current.end(), back_inserter(subindex));
        auto& here = current[0];

        if ( here >= sequence.size() ) // no more positions to match
            return check_variadic ? match_variadic(ch, current) : index_t {};
        //eslog( " --- checking with: ", sequence[here]->name() );
        eslog.level++;
        subindex = sequence[here]->match( ch, subindex );
        eslog.level--;
        if ( subindex.empty() )
            return check_variadic ? match_variadic(ch, current) : index_t {};
        // pre-reserve space in case when 'subindex' was drilled
        if ( current.size() != subindex.size()+1 )
            current.resize( subindex.size()+1 );
        if ( sequence[here]->full(subindex) )
        {
            fill(current.begin()+1, current.end(), 0);
            // XXX FIX: don't increase the position if the position
            // follows a lexeme with variadic flag
            current[0]++;
            eslog( " --- sub is matched as full, flipping to the next: ", current );
        }
        else
        {
            copy(subindex.begin(), subindex.end(), current.begin()+1);
            eslog( " --- sub not yet full, keeping its update: ", current );
        }
        return current;
    }

    index_t match_sequence( char ch, index_t current ) const
    {
        // XXX This procedure is valid only for cases when
        // whitespace is optionally empty. For a case when
        // whitespace is required, this procedure must be
        // completely different and possibly use state.
        auto end = whitespace + sizeof whitespace;
        if ( find(whitespace, end, ch) != whitespace )
        {
            // skip whitespace
            eslog( " --- incoming whitespace; skipping as for sequence" );
            return current;
        }

        return match_string( ch, current );
    }

    virtual bool full(const index_t index) const override
    {
        return !index.empty()
            and index[0] == sequence.size()
            and all_of(index.begin()+1, index.end(), [](index_t::value_type v) { return v == 0; } );
    }
	*/


    [[gnu::noreturn]]
    virtual set<char> characters() const override
    {
        throw LexemeBase::error( "Sequence lexeme cannot be asked for characters!" );
    }

    string rev_def() const
    {
        string out;
        if ( string_() )
        {
            out = " = ";
            for (auto i: sequence_)
                out += i->name() + " ";
            return out;
        }

        out = " { sequence ";
        for (auto i: sequence_)
            out += i->name() + " ";
        out += "}";
        return out;
    }
};

DirectLexeme* LexemeBase::globmatch[256] = { 0 };

// value wrapper for LexemeBase
class Lexeme
{
    LexemeBase* in;
	friend class LexemeBase;

	struct SystemLexemes;
	static SystemLexemes syslx;
	friend SystemLexemes;

	// Private constructor only for system lexemes
	Lexeme() {}

    public:

	static Lexeme any();
	static Lexeme whitespace();
	static Lexeme empty();

    using index_t = LexemeBase::index_t;
    using range_t = LexemeBase::range_t;
    using alt_t = LexemeBase::alt_t;
    using sequence_t = LexemeBase::sequence_t;
    using string_t = LexemeBase::string_t;
    using error = LexemeBase::error;

    static constexpr auto range = LexemeBase::range;
    static constexpr auto alt = LexemeBase::alt;
    static constexpr auto seq = LexemeBase::seq;
    static constexpr auto str = LexemeBase::str;


    static Lexeme direct(char ch)
    {
        LexemeBase* src = LexemeBase::globmatch[int(ch)];
		string errmsg = "No direct assignment found for: \""_S + ch + '"';
        if ( !src )
            throw error(errmsg);

        return Lexeme(src);
    }

	static LexemeBase* source(char ch)
	{
		return LexemeBase::globmatch[int(ch)];
	}

    Lexeme( LexemeBase* src ): in(src)
    {
    }

    Lexeme( string name, char c )
    {
        // Take over the existing Lexeme, if there is any.
        DirectLexeme* src = LexemeBase::globmatch[int(c)];
        if ( src )
        {
            // If found, take it over.
            in = src;
            // Change the name
            in->lexname_ = name;
            return;
        }

        // Otherwise, create a new EXCLUSIVE lexeme.
        // (note that all direct-char-assigned lexemes must be exclusive)
        src = new DirectLexeme( name, c );
        LexemeBase::globmatch[int(c)] = src;
		in = src;
        eslog( "> lexeme ", name, " = \"", c, '"' );
    }

    template<class... Args>
    Lexeme( string name, range_t rng, char from, char to, Args... args ):
        Lexeme( name, Occurrence::exact, rng, from, to, args... ) {}

    template<class... Args>
    Lexeme( string name, Occurrence oxx, range_t rng, char from, char to, Args... args )
    {
        auto cl = new CharLexeme(name, oxx, rng, from, to, args...);
        in = cl;
        eslog( "> lexeme ", name, " = ", flaggen( cl->rev_def(), oxx ) );
    }

    Lexeme( string name, range_t rng, const char* pairs ):
        Lexeme( name, Occurrence::exact, rng, pairs ) {}

    Lexeme( string name, Occurrence oxx, range_t rng, const char* pairs )
    {
        auto t = new CharLexeme(name, oxx);
        auto p = pairs;
        char from, to;

        for(;;)
        {
            if ( !*p )
                break;
            from = *p;
            ++p;
            if ( !*p )
            {
                eslog( " --- init(", name, "): adding alt: ", from );
                t->init_alt(from);
            }
            else
            {
                to = *p;
                ++p;
                eslog( " --- init(", name, "): adding range [", from, "-", to, "]" );
                t->init_range(from, to);
            }
        }

		in = t;
        eslog( "> lexeme ", name, " = ", flaggen( t->rev_def(), oxx ) );
    }

    template<class... Args>
    Lexeme( string name, Occurrence oxx, alt_t, char c, Args... args )
    {
        auto t = new CharLexeme(name, oxx, alt, c, args...);
        in = t;
        eslog( "> lexeme ", name, " = ", flaggen( t->rev_def(), oxx ) );
    }

    template<class... Args>
    Lexeme( string name, alt_t, Lexeme l, Args... args )
    {
        auto t = new AlternativeLexeme(name);
        add_alt(t, l, args...);
        in = check_single(t);
		if ( in == t )
        	eslog( "> lexeme ", name, " = ", t->rev_def() );
		else
			eslog( "> lexeme ", name, ", = /", in->characters(), "/" );
    }

    template<class... Args>
    void add_alt(AlternativeLexeme* a, Lexeme l, Args... args)
    {
        a->init(l.in);
        add_alt(a, args...);
    };

    void add_alt(AlternativeLexeme*) {}

	Lexeme( string name, Occurrence oxx, Lexeme l )
	{
		in = new RichLexeme(name, l.in, oxx);
		eslog( "> lexeme ", name, " = ", flaggen(l.name(), oxx) );
	}

    // NOTE:
    // This is specific for using already defined lexeme, but
    // only such that is already single.
    template<class... Args>
    Lexeme( string name, Occurrence flags, alt_t, Lexeme l, Args... args )
    {
        auto t = new CharLexeme(name, flags);
        add_alt(t, l, args...);
		in = t;
        string nme = "/";
        auto src = in->characters();
        copy(src.begin(), src.end(), back_inserter(nme));
        nme += "/";
        eslog( "> lexeme ", name, " = ", flaggen( nme, flags ) );
    }

    template<class... Args>
    void add_alt(CharLexeme* a, Lexeme l, Args... args)
    {
        a->import(l.in);
        add_alt(a, args...);
    };

    void add_alt(CharLexeme*) {}

    LexemeBase* check_single(AlternativeLexeme* alt)
    {
        // This function optimizes the structure of AlternativeLexeme.
        // If every underling is a single character lexeme, try to
        // unwind this into single characters, and replace this given
        // AlternativeLexeme with CharLexeme. XXX To be done.
        // Note that this may be sometimes required because
        // AlternativeLexeme can be by no means treated as "single".
        return alt;
    }

    // XXX Constructors for SequenceLexeme

    template<class... Args>
    Lexeme( string name, string_t, Lexeme l, Args... args)
    {
        SequenceLexeme* seq = new SequenceLexeme(name, l.in);
        add_seq(seq, args...);
		in = seq;
        eslog( "> lexeme ", name, seq->rev_def() );
    }

    template<class... Args>
    Lexeme( string name, sequence_t, Lexeme l, Args... args)
    {
        SequenceLexeme* seq = new SequenceLexeme(name, l.in);
        add_seq(seq, args...);
        seq->string_(false);
		in = seq;
        eslog( "> lexeme ", name, seq->rev_def() );
    }

    template<class... Args>
    void add_seq(SequenceLexeme* a, Lexeme l, Args... args)
    {
        a->init(l.in);
        add_seq(a, args...);
    };

    void add_seq(SequenceLexeme*) {}

	bool extends(Lexeme l) { return in->extends(l); }
    // Exposure
	// (not possible to implement - must be remembered when it happens)
    // bool full(index_t position) const { return in->full(position); }
	LexemeBase::Match match( char ch, index_t& current, bool& full ) const
    {
        eslog( "MATCH ", name(), ": '", ch, "' @ ", current );
        return in->match( ch, current, full );
    }
    Occurrence flags() { return in->flags(); }
    string show_structure() { return in->show_structure(); }

    template<class C>
    void get_extenders(C& result)
    {
        transform(in->extenders_.begin(), in->extenders_.end(), back_inserter(result),
                [](LexemeBase* l) { return Lexeme(l); } );
    }

    template<class C>
    void get_followers(C& result)
    {
        transform(in->followers_.begin(), in->followers_.end(), back_inserter(result),
                [](LexemeBase* l) { return Lexeme(l); } );
    }

    bool single() { return in->single(); }

    string name() const
    {
        if ( !in )
            return "<\?\?\?>";
        return in->name();
    }
    bool operator<(const Lexeme& right) const { return in < right.in; }
    bool operator==(const Lexeme& right) const { return in == right.in; }

    operator bool () { return in; }
};

const Lexeme& Lexeme_wrap( LexemeBase* ptr) { return Lexeme(ptr); }

ostream& operator<<(ostream& sout, const Lexeme& lx)
{
	sout << lx.name();
	return sout;
}
// Orphans
inline bool LexemeBase::extends(Lexeme l) const
{
	if ( l.in == this )
		return true;

	for (auto xtd: extends_)
	{
		if ( xtd.extends(l) )
			return true;
	}
	return false;
}


// This overload requires Lexeme defined.
string flaggen( Lexeme d ) { return flaggen( d.name(), d.flags()); }
inline string make_printable(const Lexeme& l) { return l.name(); }
template<class Container> inline
void inplace_unique(Container& c)
{
    c.erase(unique(c.begin(), c.end()), c.end());
}


void error(const string& msg)
{
    cerr << "Error: " << msg << endl;
    abort();
}

class Scanner
{
public:
    using index_t = Lexeme::index_t;
    struct Candidate
    {
        Lexeme lexeme = Lexeme::empty();
        index_t position = { 1 };
        //index_t position = index_t(1u, 1u);
        bool isfull = false;
		LexemeBase::Match matchresult;
		bool isready = false;
        string characters;

		Candidate() = default;
        Candidate( Lexeme l ): lexeme(l)
		{
		}
        bool full() { return isfull; }
        bool operator<(const Candidate& right) const
        { return lexeme < right.lexeme; }

        bool operator==(const Candidate& right) const { return lexeme == right.lexeme; }

		LexemeBase::Match match(char inch)
        {
			matchresult = lexeme.match(inch, position, isfull);
			if ( matchresult == LexemeBase::Match::found )
			{
				characters.push_back(inch);
			}
			return matchresult;
        }
    };

	struct Fixed
	{
		Candidate prefix;
		vector<char> characters;

		Fixed() = default;
		Fixed(Candidate&& src): prefix( move(src) ) {}
		Fixed(const Candidate& src): prefix(src) {}
	};

	struct MatchTree
	{
		Lexeme root = Lexeme::empty(); // a DirectLexeme for which the following candidates were picked up
		list<Candidate> pending;
		list<Fixed> fixed;

		void clear()
		{
			pending.clear();
			fixed.clear();
			root = Lexeme::empty();
		}

		MatchTree() = default;
		MatchTree(const Candidate& src) = delete; //: pending(src) {}

		void filter(const vector<Lexeme>& expected, list<Candidate>& dropped)
		{
			// When expected is empty, do not check for expected.
			if ( expected.empty() )
				return;

			for (auto pf = fixed.begin(), next = pf; pf != fixed.end(); pf = next )
			{
				++next;
				// Filter out unexpected
				auto fif = find_if(expected.begin(), expected.end(), [pf](Lexeme x) {
						return x.extends(pf->prefix.lexeme);
						});
				if ( fif == expected.end() )
				{
					eslog( " --- Dropping from pending: not expected" );
					fixed.erase(pf);
					continue;
				}

			}

			auto newend = remove_if(pending.begin(), pending.end(),
					[] (const Candidate& cand) { return cand.isready; } );
			dropped.splice(dropped.end(), pending, newend, pending.end());
		}

	};

	// This should look like:
	//
	// (after incoming: a a)
	// [a] (direct lexeme)
	// candidates: aaa aab (abc - dropped)
	// prefix: a
	// follow = {
	//     candidates: aaa aab
	//     prefix: (empty)
	//     follow: null
	// }
	// It means that:
	// - we consider aaa and aab, after we confirmed [aa] part of both
	// - but we have also found single [a] and the next [a] can be also the beginning of another lexeme
	// - so, we have [a] as a fixed lexeme and a list of candidates starting from [a]
	// - if the next character would be c then:
	//   - both candidates of the main list will be dropped as not matching (there's no 'aac...')
	//   - after that, the main list will be overwritten by the follower list, @char:a is appended to the output
	//   - among the follower list there's still no candidate that would match (there's no 'ac...')
	//   - so this list is also flushed ( @char:a is appended to the output)
	//   - the 2nd level list has no followers, so the state is set to clear

    void fill_candidates(list<Candidate>& cand, Lexeme direct, char inch)
    {
		// This gets the first-level followers and all possible extenders.
		auto&& candidates = select_candidates(direct);
		eslog( " --- + Selected ", candidates.size(), " candidates:" );
		for_each(candidates.begin(), candidates.end(), [inch](Candidate& c) {
					c.characters.push_back(inch); // send the first character
					eslog( " --- +++ ", c.lexeme.name() );
				});
		cand.splice(cand.end(), candidates);

        // Ok, so now add the direct assignment to candidates.
        cand.emplace_back( direct );

        cand.sort();
        inplace_unique(cand);
    }

    bool clear = true;

	// These are direct candidates, which follow the first found character.
	// The inside candidate list follows an already full longest (last found)
	// lexeme, which matches previous characters. This train can be flushed
	// only when the main candidate list becomes empty. Then every follower
	// does the same: when its candidate list is empty, it's flushed, and
	// its own candidate list overrides the main candidate list.
    MatchTree mt;
	Lexeme selected_lexeme = Lexeme::empty();

    /// When a lexeme is dropped from candidates, it lands first here.
    /// The dropped list is cleared at every time when accept() is called.
    /// This allows to keep track on what candidates were dropped last time.
	// XXX
    list<Candidate> dropped;

public:


	bool accept( char inch, const vector<Lexeme>& expected, vector<Candidate>& result );

    list<Candidate> select_candidates(Lexeme base);

    //Lexeme selected_lexeme() { return fixed.match; }
    //string selected_characters() { return fixed.characters; }

    string show_error()
    {
        // this function returns a string that describe what happened
        // that made selection not found and no more candidates.
        string out;

        if ( !mt.pending.empty() )
        {
            out += "Multiple match: ";
            for (auto k: mt.pending)
            {
                out += k.lexeme.show_structure() + "; ";
            }
        }

        out += "Last dropped: ";
        for (Candidate k: dropped)
        {
            out += k.lexeme.show_structure() + "; ";
        }
        return out;
    }

};

inline string make_printable(const list<Scanner::Candidate>& l )
{
    string out;
    for (auto i: l)
        out += " " + flaggen( i.lexeme );
    return out;
}

inline string make_printable(const Scanner::index_t& ix)
{
	ostringstream os;
	for ( auto i: ix )
	{
		os << "[" << i << "]";
	}
	return os.str();
}

list<Scanner::Candidate> Scanner::select_candidates(Lexeme base)
{
    // 1. Select those lexemes, for which this is the first one.
    // Should be noted in its "followers"

	eslog( " --- :Selecting candidates for direct: ", base.name(), " -- followers:" );

    list<Candidate> result;
    base.get_followers(result);
	
	for (const auto& r: result)
		eslog( " --- ... ", r.lexeme.name() );

    // 2. Look for extenders (those for which this one is one of alternatives)
    // and add them to the considerations too.

	eslog( " --- : ... extenders:" );
    vector<Lexeme> extenders;
    base.get_extenders(extenders);
    for (auto axl: extenders)
    {
		eslog( " --- ..... ", axl.name(), " --- its candidates:" );
        auto&& local = select_candidates(axl);
        move( local.begin(), local.end(), back_inserter(result) );
		// The extender itself is not added to the candidates,
		// it will be only next available for extraction.
    }
	eslog( " --- : ... (", base.name() , ") total of ", result.size(), " elements" );

    return result;
}

bool Scanner::accept( char inch, const vector<Lexeme>& expected, vector<Candidate>& result )
{
    // XXX do some pre-clean
    //static char prev;
	static string collected_chars;

    Lexeme direct = Lexeme::direct(inch);
    eslog( "++ Direct: ", direct.name());

	using Match = LexemeBase::Match;

	// Note:
	// 'Lexeme': the pattern that has to be matched.
	// 'Candidate': a Lexeme with current its own index of last match and other data
	// 'Fixed': a matched Candidate 

	collected_chars.push_back( inch );
    // If we're at the beginning
    if ( mt.root == Lexeme::empty() )
    {
        eslog( " --- Refilling candidates:" );
		mt.root = direct;
        fill_candidates(mt.pending, direct, inch);
        dropped.clear();
        eslog( "Candidates: ", mt.pending);

        // Now make an overview to check for full.

		// Ok, now check if we have any remaining candidates :)
		if ( mt.pending.empty() )
			return true; // return "please read" state, but do not put anything to results
		// this will have to be understood that nothing can be matched - found a syntax error

		// This actually should never happen because at least 'direct' will be a
		// candidate. It's also a full candidate, so let's set it to pending.

		// direct must be at least one candidate, so if size() == 1, this is it.
		// In this case, we're ready to report.
		if ( mt.pending.size() == 1 )
		{
			// filter this one before reporting
			mt.filter(expected, dropped);
			if ( mt.pending.empty() )
			{
				eslog( "... Returning empty result!" );
				mt.root= Lexeme::empty();
				return true;
			}
			// Take the result immediately and clear.
			eslog( "... Returning result: ", mt.pending);
			move(mt.pending.begin(), mt.pending.end(), back_inserter(result));
			mt.root= Lexeme::empty();
			return true;
		}
		return false; // all candidates are pending (except this one direct)
    }

	eslog( "Remaining candidates: ", mt.pending );

	// If we're here, the situation is:
	// - at least 1 lexeme in candidates
	// - at least 1 character confirmed, we are at at least [1] position

    // XXX Lexemes collected here will have to be consolidated.

    // Otherwise we're already in the middle, so we're searching through the candidates.

    // Processing of particular list of candidates can result in different posteractions.
    // But anyway first thing to do is to check if the character is capable of matching
    // any of lexemes in the list.

    // The candidate, that doesn't have a matching lexeme in its list, is dropped.
    // When a lexeme has been considered matched completely, a list of candidates
    // is extended with its logical includers.
    // When there is at least one candidate with MOST or FULL state, and the next
    // character does not match any of candidates, the best candidate is selected
    // as this match. If more than one is seen, then string precedes sequence and
    // FULL precedes MOST. If there are still more than 1, a conflict error is reported.

    // Now update the state. We are now in the middle of some lexeme.

	do
	{
//repeat: ;

		// This loop continues, when
		// - the list of candidates at that level is nonempty
		// - none of candidates is full (if full, then set this to prefix, and delete all following)
		eslog( "++ Testing candidates:" );
		for (auto pc = mt.pending.begin(), next = pc; pc != mt.pending.end(); pc = next)
		{
			++next;

			// Test candidates for possible dropping.
			Match match = pc->match(inch);

			// Resulting situation is:
			// - Match::found: still in progress
			// - Match::failed: drop candidate
			// - Match::full: 'inch' can be no longer any additional matching character.

			// - pc->full() == true means that after the current 'inch', this candidate
			//   is a full match. Combination with Match:
			//   - Match::full: possible, but should be eliminated by moving to fixed!
			//   - Match::failed: not possible
			//   - Match::found: in that case it should be moved to fixed immediately.

			// It is possible that lexemes moved to fixed can still consume characters.
			// Even if not, they would be otherwise put as fixee's pending.

			// if not pc->full():
			//    - Match::failed: drop candidate
			//    - Match::found: match in progress, leave in candidates
			//    - Match::full: possible only in two cases:
			//      - previously it was 'found': should've been moved to fixed!
			//      - neither one character matched, but all were optional: same as 'failed'.

			// What to do:
			// if pc->full():
			//   - Match::full: drop
			//   - Match::failed: not possible (drop if found, but report internal error)
			//   - Match::found: move to fixed
			// Otherwise:
			// - Match::full or Match::failed - drop
			// - Match::found: do nothing

			if ( pc->full() )
			{
				// Check if maybe there's already one full matching.

				if ( match == Match::found )
				{
					eslog( "-- '", pc->lexeme, "': no more characters, but accepted - setting to fixed" );
					pc->isready = true; // Move to fixed later
				}
				else
				{
					// This branch should never happen because:
					// - Match::failed should not happen when pc->full().
					// - Match::full should never be achieved at 'pending' (at worst at fixed)
					if ( match == Match::failed )
					{
						eslog( " !!! INTERNAL ERROR: full+failed - ", pc->lexeme );
					}
					eslog( "-- '", pc->lexeme, "' cannot be matched once again: already full - dropping" );
					dropped.splice(dropped.end(), mt.pending, pc, next);
				}
				// SINCE NOW pc is no longer valid!
				continue;
			}
			else if ( match != Match::found )
			{
				// Here only 'failed' should be possible because those with 'full'
				// should be now moved to 'fixed' and no longer be in 'pending'.
				eslog( "-- '", pc->lexeme, "': no more characters and not fully matched - dropping" );
				// tear off *pc from candidates and put it into dropped.
				// The "next" is left untouched in this operation.
				dropped.splice(dropped.end(), mt.pending, pc, next);
			}

		}

		// Ok, now do the same for the "fixed" from the old cast

		for (auto& imt: mt.fixed)
		{
			// First check the current "prefix". This is already full(), but maybe it will
			// consume additional characters (has some optionals).
			// Do that only for those, whose list of characters is empty becuase
			// otherwise they are already collecting their follower characters.
			if ( imt.characters.empty() )
			{
				Match mr = imt.prefix.match(inch);
				// If succeeded, keep the root empty.
				if ( mr == Match::found )
				{
					eslog( "FIXED still consuming '", inch, "': ", imt.prefix.lexeme );
					continue;
				}
			}
			// mr is either 'found' of 'full'; when the second occurs it means that
			// the current character was not accepted by the lexeme, so it should be
			// added to the tail.
			// If the tail was not empty then of course add it anyway.
			imt.characters.push_back(inch);

			eslog( "FIXED full: ", imt.prefix.lexeme, ": adding first follower character '", inch , "'" );
			// Current 'inch' is just used to generate the candidate list, so there's
			// nothing more to do here.
		}

		// Ok, now move candidates -> fixed for those that isready.
		for (auto pc = mt.pending.begin(), next = pc; pc != mt.pending.end(); pc = next )
		{
			++next;
			if ( pc->isready )
			{
				eslog( "-- Setting as fixed: ", pc->lexeme );
				move( pc, next, back_inserter(mt.fixed));
				mt.pending.erase(pc);
			}
		}

		//prev = inch;

		// GOOD. Now the state of all pending and fixed is up to date.
		// Values from matchresult in the candidate can be taken as
		// a good deal. 

		// Check if the list of candidates is exhausted.
		// If so, take the last fixed, unless unexpected.
		if ( mt.pending.empty() )
		{
			eslog( "-- No more candidates, let's see if we matched anything" );
			mt.filter(expected, dropped);
			if ( mt.fixed.empty() )
			{
				eslog( "-- Nothing in fixed - report empty match" );
				// Nothing  matches and no more candidates.
				// XXX consider fallback
				selected_lexeme = Lexeme::empty();
				mt.root= Lexeme::empty();
				return true;
			}
			else
			{
				// Review the fixed to see what was their match result. So:
				// - some of the fixed have been moved to fixed at this step
				// - some others were matched previously, but still consume incoming characters
				// - all others were matched previously and have nonempty tail

				// What to do:
				// 1. Check if there is any fixed with empty tail. If it is, then
				//    leave in the curret state because this one has a potential of
				//    consuming additional characters.
				// 2. Before leaving, delete all fixed candidates that have nonempty
				//    tail. They will not be very useful in the next iteration.

				auto pe = find_if(mt.fixed.begin(), mt.fixed.end(), [](const Fixed& f) {
							return f.characters.empty();
						});
				if ( pe != mt.fixed.end() )
				{
					eslog( "Found one fixed with no trailer: ", pe->prefix.lexeme, " - removing others, still pending" );
					auto newend = remove_if(mt.fixed.begin(), mt.fixed.end(), [](const Fixed& f) {
								return !f.characters.empty();
							});
					mt.fixed.erase(newend, mt.fixed.end());
					break; // the block
				}

				// Here we are with the situation that every fixed has at least 1
				// character as a trailer.

				// Take the first longest fixed. The "longest" lexeme is the one
				// that's character trailer list is the shortest. May be more
				// than one element, so first find the minimum, then the range of minimum,
				// then the first of the range.
				auto has_less_characters = [] (const Fixed& f1, const Fixed& f2) {
						return f1.characters.size() < f2.characters.size();
				};

				mt.fixed.sort(has_less_characters);
				auto imin = min_element(mt.fixed.begin(), mt.fixed.end(), has_less_characters);
				auto rng = equal_range(mt.fixed.begin(), mt.fixed.end(), *imin, has_less_characters);
				result.push_back( rng.first->prefix );
				selected_lexeme = rng.first->prefix.lexeme;

				eslog( "-- Taking ", selected_lexeme, " as match and reporting" );

				// Now copy the follower characters that were accidentally buffered,
				// in order to stuff them again into the input.
				vector<char> pending_inch = move(imin->characters);
				// Now clear all
				mt.clear();
				// And repeat match for the characters collected so far.
				if ( !pending_inch.empty() )
				{
					eslog( "-- ** Repeating accept() for the remaining characters: ", pending_inch );
					for (auto einch: pending_inch )
					{
						vector<Candidate> eresult;
						if ( accept(einch, expected, eresult) )
						{
							// Locally, it has matched some result, add it to the results.
							move(eresult.begin(), eresult.end(), back_inserter(result));
							eresult.clear(); // mt will be reset automatically whenever true is returned.
						}
					}
					eslog( "-- ** Back to the input-pending mode" );
				}
			}

			collected_chars.clear();
			mt.root= Lexeme::empty();
			return true;
		}
	} while(0); // repeatable block

    eslog( "-- Nothing to report, but still have ", mt.pending.size()," candidates and ", mt.fixed.size(), " fixed." );
	// If candidates not empty, continue trying.
	return false; // did not elliminate all candidates yet, continue
}

    ////////////////////////////////////////////////////////////////////////
   ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////
 ////////////////////////////////////////////////////////////////////////

template<typename Val>
struct IstreamRange
{
    istream& in;
    IstreamRange(istream& x): in(x)
	{
		in.unsetf(ios::skipws);
	}
    istream_iterator<Val> begin() { return istream_iterator<Val>(in); }
    istream_iterator<Val> end() { return istream_iterator<Val>(); }
};

struct Lexeme::SystemLexemes
{
	Lexeme whitespace;
	Lexeme any;
	Lexeme empty;
	vector<Lexeme> direct;
	SystemLexemes();
};

Lexeme::SystemLexemes Lexeme::syslx;

Lexeme::SystemLexemes::SystemLexemes()
{
	eslog( "Initializing system lexemes" );
	// Whitespace: copy the whitespace variable
	// Remember: it has "optional" flag XXX
	// and currently it can't be skipped!
	CharLexeme* in = new CharLexeme( "@whitespace", Occurrence::optional );

	for (const char* pc = ::whitespace; pc != ::whitespace + (sizeof(::whitespace)-1); ++pc)
		in->init_alt(*pc);
	whitespace = Lexeme(in);
	eslog( " --- @whitespace: ", in->rev_def() );

	// Any: no, it's not "every possible character in the world".
	// These are all characters that are considered printable characters
	// and therefore can be used in a language definition.

	in = new CharLexeme( "@any", Occurrence::exact );

	for (char i = '\0'; i < 127; ++i) // ASCII only. We'll think later what about UTF-8.
		if ( isprint(i) )
		{
			in->init_alt(i);
		}

	// It might have been a good idea to optimize it to use ranges,
	// we'll think about this later.
	any = Lexeme(in);

	empty = Lexeme(new EmptyLexeme( "@empty" ));
}

Lexeme Lexeme::any() { return syslx.any; }
Lexeme Lexeme::whitespace() { return syslx.whitespace; }
Lexeme Lexeme::empty() { return syslx.empty; }

int main( int argc, char** argv )
{
	cout << "INITIALIZATION:\n";
    Scanner scanner;

    // Our example grammar
    Lexeme plus {  "plus", '+' };
    Lexeme minus {  "minus", '-' };
    Lexeme multiply {  "multiply", '*' };
    Lexeme divide {  "divide", '/' };

    Lexeme alpha {  "alpha", Lexeme::range, "azAZ_" };
    Lexeme cipher {  "cipher", Lexeme::range, "09" };

    // lexeme alnum = alpha|cipher
    Lexeme alnum (  "alnum", Lexeme::alt, alpha, cipher );

    // lexeme langword { string alpha alnum...? }
    // INTERMEDIATE
    Lexeme _t_alnum_variadic_optional {  "@gen:alnum:r:variadic:optional", Occurrence::variadic | Occurrence::optional, alnum };
    Lexeme langword {  "langword", Lexeme::str, alpha, _t_alnum_variadic_optional};

	// Pascal string
	Lexeme quote { "quote", '\'' };
	Lexeme quote_quote { "quote_quote", Lexeme::str, quote, quote };
	Lexeme not_quote { "not_quote", Occurrence::inverted, quote };
	Lexeme in_pstring_ch { "in_pstring_ch", Lexeme::alt, not_quote, quote_quote };
	Lexeme _t_in_pstring_ch_variadic_optional ( "in_pstring_ch...?", Occurrence::variadic | Occurrence::optional, in_pstring_ch );
	Lexeme pstring { "pstring", Lexeme::str, quote, _t_in_pstring_ch_variadic_optional, quote };

	cout << "FOLLOWER STRUCTURE DUMP:\n";
	for (char c = 1; c < 127; ++c)
	{
		LexemeBase* lx = Lexeme::source(c);
		if ( !lx )
		{
			cout << "&" << int(c) << " not valid\n";
			continue;
		}
		cout << lx->name() << ": ";

		cout << "EXT ";
		for (auto x: lx->extenders_)
			cout << x->name() << " ";
		cout << "FLW ";
		for (auto f: lx->followers_)
			cout << f->name() << " ";
		cout << endl;
	}

	vector<Scanner::Candidate> results;
	//for_each( istream_iterator<char>(cin), istream_iterator<char>(), [&scanner](char inch) \{ \}
    for ( char inch: IstreamRange<char>(cin) )
    {
		try
		{ // the default system handler should do the thing I need
			// but somehow it doesn't print the what() contents...

			cout << "SENDING CHARACTER: " << inch << endl;
			if ( !scanner.accept(inch, vector<Lexeme> {}, results) )
				continue;
		}
		catch (const std::exception& x)
		{
			cerr << "BROKEN ON EXCEPTION: " << x.what() << endl;
			raise(SIGQUIT); // force dumping stacktrace in debugger
		}

        // Check if the candidate was selected; if not, report error.
        if ( !results.empty() )
        {
			cerr << "Received results:\n";
			for (auto& cand: results)
			{
				Lexeme l = cand.lexeme;
				cerr << "MATCHED LEXEME: " << l.name() << " : " << cand.characters << endl;
				vector<Lexeme> extenders;
				l.get_extenders(extenders);
				if ( !extenders.empty() )
				{
					cerr << " --- alternatives: ";
					transform(extenders.begin(), extenders.end(), ostream_iterator<string>(cout, " "),
							[] (Lexeme lx) { return lx.name(); } );
					cerr << endl;
				}
			}
            continue;
        }

        cout << "Syntax error:\n" << scanner.show_error() << endl;
        break;
    }

    return 0;
}

// DEBUG SUPPORT
void show_pending(const list<Scanner::Candidate>& lst)
{
	for (const auto& p: lst)
	{
		eslog( "P: ", p.lexeme.name(), " : ", p.position );
	}
}

