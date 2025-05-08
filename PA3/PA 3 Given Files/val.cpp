#include "val.h"
#include <cmath>
#include <cstdlib>

Value Value::operator+(const Value& op) const {
	if (GetType() == VINT && op.GetType() == VINT) {
		return Value(GetInt() + op.GetInt());
	}
	else if (GetType() == VREAL && op.GetType() == VREAL) {
		return Value(GetReal() + op.GetReal());
	}

	return Value();
}

Value Value::operator-(const Value& op) const {
	if (GetType() == VINT && op.GetType() == VINT) {
		return Value(GetInt() - op.GetInt());
	} 
	else if (GetType() == VREAL && op.GetType() == VREAL) {
		return Value(GetReal()- op.GetReal());
	}

	cerr << "ERROR2";
	return Value();
}

Value Value::operator*(const Value& op) const {
	if (GetType() == VINT && op.GetType() == VINT) {
		return Value(GetInt() * op.GetInt());
	}
	else if (GetType() == VREAL && op.GetType() == VREAL) {
		return Value(GetReal() * op.GetReal());
	}

	return Value();
}

Value Value::operator/(const Value& op) const {
	if (GetType() == VINT && op.GetType() == VINT) {
		return Value(GetInt() / op.GetInt());
	}
	else if (GetType() == VREAL && op.GetType() == VREAL) {
		return Value(GetReal() / op.GetReal());
	}

	cerr << "ERROR4";
	return Value();
}

Value Value::operator%(const Value& op) const {
	if (GetType() == VINT && op.GetType() == VINT) {
		return Value(GetInt() % op.GetInt());
	}

	cerr << "ERROR5";
	return Value();
}

Value Value::operator==(const Value& op) const {
	if (GetType() != op.GetType()) {
		cerr << "ERROR6";
		return Value();
	}

	switch(GetType()) {
		case VINT:
			return GetInt() == op.GetInt();
		case VREAL:
			return GetReal() == op.GetReal();
		case VSTRING:
			return GetString() == op.GetString();
		case VCHAR:
			return GetChar() == op.GetChar();
		case VBOOL:
			return GetBool() == op.GetBool();
		default:
			cerr << "ERROR7";
			return Value();
	}
}

Value Value::operator!=(const Value& op) const {
	if (GetType() != op.GetType()) {
		cerr << "ERROR8";
		return Value();
	}

	switch(GetType()) {
		case VINT:
			return GetInt() != op.GetInt();
		case VREAL:
			return GetReal() != op.GetReal();
		case VSTRING:
			return GetString() != op.GetString();
		case VCHAR:
			return GetChar() != op.GetChar();
		case VBOOL:
			return GetBool() != op.GetBool();
		default:
			cerr << "ERROR9";
			return Value();
	}
}

Value Value::operator>(const Value& op) const {
	if (GetType() != op.GetType()) {
		cerr << "ERROR10";
		return Value();
	}

	switch(GetType()) {
		case VINT:
			return GetInt() > op.GetInt();
		case VREAL:
			return GetReal() > op.GetReal();
		case VSTRING:
			return GetString() > op.GetString();
		case VCHAR:
			return GetChar() > op.GetChar();
		case VBOOL:
			return GetBool() > op.GetBool();
		default:
			cerr << "ERROR11";
			return Value();
	}
}

Value Value::operator<(const Value& op) const {
	if (GetType() != op.GetType()) {
		cerr << "ERROR12";
		return Value();
	}

	switch(GetType()) {
		case VINT:
			return GetInt() < op.GetInt();
		case VREAL:
			return GetReal() < op.GetReal();
		case VSTRING:
			return GetString() < op.GetString();
		case VCHAR:
			return GetChar() < op.GetChar();
		case VBOOL:
			return GetBool() < op.GetBool();
		default:
			cerr << "ERROR13";
			return Value();
	}
}

Value Value::operator<=(const Value& op) const {
	if (GetType() != op.GetType()) {
		cerr << "ERROR14";
		return Value();
	}

	switch(GetType()) {
		case VINT:
			return GetInt() <= op.GetInt();
		case VREAL:
			return GetReal() <= op.GetReal();
		case VSTRING:
			return GetString() <= op.GetString();
		case VCHAR:
			return GetChar() <= op.GetChar();
		case VBOOL:
			return GetBool() <= op.GetBool();
		default:
			cerr << "ERROR15";
			return Value();
	}
}

Value Value::operator>=(const Value& op) const {
	if (GetType() != op.GetType()) {
		cerr << "ERROR16";
		return Value();
	}

	switch(GetType()) {
		case VINT:
			return GetInt() >= op.GetInt();
		case VREAL:
			return GetReal() >= op.GetReal();
		case VSTRING:
			return GetString() >= op.GetString();
		case VCHAR:
			return GetChar() >= op.GetChar();
		case VBOOL:
			return GetBool() >= op.GetBool();
		default:
			cerr << "ERROR17";
			return Value();
	}
}

Value Value::operator&&(const Value& op) const {
	if (GetType() != VBOOL || op.GetType() != VBOOL) {
		cerr << "ERROR18";
		return Value();
	}

	return Value(GetBool() && op.GetBool());
}

Value Value::operator||(const Value& op) const {
	if (GetType() != VBOOL || op.GetType() != VBOOL) {
		cerr << "ERROR19";
		return Value();
	}

	return Value(GetBool() || op.GetBool());
}

Value Value::operator!(void) const {
	if (GetType() != VBOOL) {
		return Value();
	}

	return Value(!GetBool());
}

Value Value::Concat(const Value& op) const {
	if ((GetType() != VSTRING && GetType() != VCHAR) || (op.GetType() != VSTRING && op.GetType() != VCHAR)) {
	cerr << "ERROR21";
	return Value();
	}

	string l, r;

	if (GetType() == VSTRING) {
		l = GetString();
	}
	else if (GetType() == VCHAR) {
		l = string(1, GetChar());
	}

	if (op.GetType() == VSTRING) {
		r = op.GetString();
	}
	else if (op.GetType() == VCHAR) {
		r = string(1, op.GetChar());
	}

	return Value(l + r);
}

Value Value::Exp(const Value& op) const {
	if (GetType() != VREAL || op.GetType() != VREAL) {
		cerr << "ERROR";
		return Value();
	}

	if (op.GetReal() == 0) {
		return Value(1);
	}
	if (GetReal() == 0) {
		return Value(0);
	}
	if (op.GetReal() < 0) {
		float res = pow(GetReal(), op.GetReal() * -1);
		return Value(1 / res);
	}
	
	return Value(pow(GetReal(), op.GetReal()));
}
