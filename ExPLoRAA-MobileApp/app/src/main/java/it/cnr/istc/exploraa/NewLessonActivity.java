package it.cnr.istc.exploraa;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.TextView;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import it.cnr.istc.exploraa.api.LessonModel;

public class NewLessonActivity extends AppCompatActivity implements AdapterView.OnItemSelectedListener, TextWatcher {

    private static final String TAG = "NewLessonActivity";
    private static final int OPEN_DOCUMENT_REQUEST_CODE = 42;
    private Spinner new_lesson_type_spinner;
    private ArrayAdapter<LessonModel> adapter;
    private EditText new_lesson_name;
    private Button add_lesson_button;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_new_lesson);

        new_lesson_type_spinner = findViewById(R.id.new_lesson_type_spinner);
        new_lesson_name = findViewById(R.id.new_lesson_name);
        add_lesson_button = findViewById(R.id.add_lesson_button);

        final List<LessonModel> models = new ArrayList<>(ExPLoRAAContext.getInstance().getModels());
        adapter = new ArrayAdapter<LessonModel>(this, 0, models) {

            @Override
            public View getDropDownView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
                return getCustomView(position, convertView, parent);
            }

            @NonNull
            @Override
            public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
                return getCustomView(position, convertView, parent);
            }

            public View getCustomView(int position, View convertView, ViewGroup parent) {
                View listItem = convertView;
                if (listItem == null)
                    listItem = LayoutInflater.from(getContext()).inflate(R.layout.lesson_model_row, parent, false);

                LessonModel model = models.get(position);

                TextView lesson_model_name = listItem.findViewById(R.id.lesson_model_name);
                lesson_model_name.setText(model.name);

                return listItem;
            }
        };
        new_lesson_type_spinner.setAdapter(adapter);

        new_lesson_name.addTextChangedListener(this);
    }

    public void add_lesson_type(View view) {
        Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT);
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        intent.setType("*/*");
        startActivityForResult(intent, OPEN_DOCUMENT_REQUEST_CODE);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == OPEN_DOCUMENT_REQUEST_CODE && resultCode == Activity.RESULT_OK) {
            Uri uri = null;
            if (data != null) {
                uri = data.getData();
                try {
                    final BufferedReader reader = new BufferedReader(new InputStreamReader(getContentResolver().openInputStream(uri)));
                    final LessonModel model = ExPLoRAAContext.GSON.fromJson(reader, LessonModel.class);
                    adapter.add(model);
                    ExPLoRAAContext.getInstance().addModel(model);
                    new_lesson_type_spinner.setSelection(adapter.getCount() - 1);
                } catch (FileNotFoundException e) {
                    Log.i(TAG, "Uri: " + uri.toString(), e);
                }
            }
        }
    }

    public void add_lesson(View view) {
        ExPLoRAAContext.getInstance().addTeachingLesson(this, new_lesson_name.getText().toString(), (LessonModel) new_lesson_type_spinner.getSelectedItem());
    }

    @Override
    public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
        add_lesson_button.setEnabled(new_lesson_name.getText().length() != 0);
    }

    @Override
    public void onNothingSelected(AdapterView<?> parent) {
        add_lesson_button.setEnabled(false);
    }

    @Override
    public void beforeTextChanged(CharSequence s, int start, int count, int after) {
    }

    @Override
    public void onTextChanged(CharSequence s, int start, int before, int count) {
    }

    @Override
    public void afterTextChanged(Editable s) {
        add_lesson_button.setEnabled(s.length() != 0 && new_lesson_type_spinner.getSelectedItem() != null);
    }
}
