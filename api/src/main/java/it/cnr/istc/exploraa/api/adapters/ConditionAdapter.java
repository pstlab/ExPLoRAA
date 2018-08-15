package it.cnr.istc.exploraa.api.adapters;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import it.cnr.istc.exploraa.api.LessonModel.Condition;

/**
 * ConditionAdapter
 */
public class ConditionAdapter extends TypeAdapter<Condition> {

    @Override
    public void write(JsonWriter out, Condition value) throws IOException {
        out.beginObject();
        out.name("type").value(value.type.name());
        switch (value.type) {
        case And:
            out.name("conditions");
            out.beginArray();
            for (Condition condition : ((Condition.AndCondition) value).conditions)
                write(out, condition);
            out.endArray();
            break;
        case Or:
            out.name("conditions");
            out.beginArray();
            for (Condition condition : ((Condition.AndCondition) value).conditions)
                write(out, condition);
            out.endArray();
            break;
        case Not:
            write(out.name("conditions"), ((Condition.NotCondition) value).condition);
            break;
        case Numeric:
            out.name("numeric_condition_type")
                    .value(((Condition.NumericCondition) value).numeric_condition_type.name());
            out.name("variable").value(((Condition.NumericCondition) value).variable);
            out.name("value").value(((Condition.NumericCondition) value).value);
            break;
        case Nominal:
            out.name("variable").value(((Condition.NominalCondition) value).variable);
            out.name("value").value(((Condition.NominalCondition) value).value);
            break;
        }
        out.endObject();
    }

    @Override
    public Condition read(JsonReader in) throws IOException {
        Condition c = null;
        in.beginObject();
        in.nextName();
        switch (Condition.ConditionType.valueOf(in.nextString())) {
        case And:
            List<Condition> and_conditions = new ArrayList<>();
            in.beginArray();
            while (in.peek() != JsonToken.END_ARRAY)
                and_conditions.add(read(in));
            in.endArray();
            c = new Condition.AndCondition(and_conditions);
            break;
        case Or:
            List<Condition> or_conditions = new ArrayList<>();
            in.beginArray();
            while (in.peek() != JsonToken.END_ARRAY)
                or_conditions.add(read(in));
            in.endArray();
            c = new Condition.OrCondition(or_conditions);
            break;
        case Not:
            in.nextName();
            c = new Condition.NotCondition(read(in));
            break;
        case Numeric: {
            Condition.NumericCondition.NumericConditionType n_cond_type = null;
            String variable = null;
            double value = -1;
            while (in.hasNext())
                switch ((in.nextName())) {
                case "numeric_condition_type":
                    n_cond_type = Condition.NumericCondition.NumericConditionType.valueOf(in.nextString());
                    break;
                case "variable":
                    variable = in.nextString();
                    break;
                case "value":
                    value = in.nextDouble();
                    break;
                }
            c = new Condition.NumericCondition(n_cond_type, variable, value);
            break;
        }
        case Nominal: {
            String variable = null;
            String value = null;
            while (in.hasNext())
                switch ((in.nextName())) {
                case "variable":
                    variable = in.nextString();
                    break;
                case "value":
                    value = in.nextString();
                    break;
                }
            c = new Condition.NominalCondition(variable, value);
            break;
        }
        }
        in.endObject();
        return c;
    }
}