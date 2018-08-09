package it.cnr.istc.exploraa;

import android.content.Intent;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.Window;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.TextView;

public class URLStimulusActivity extends AppCompatActivity {

    private TextView url_stimulus_content;
    private WebView url_stimulus_web_view;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_urlstimulus);

        getWindow().requestFeature(Window.FEATURE_PROGRESS);

        url_stimulus_content = findViewById(R.id.url_stimulus_content);
        url_stimulus_web_view = findViewById(R.id.url_stimulus_web_view);

        onNewIntent(getIntent());
    }

    @Override
    protected void onNewIntent(Intent intent) {
        url_stimulus_content.setText(intent.getStringExtra("content"));

        url_stimulus_web_view.setWebChromeClient(new WebChromeClient() {
            public void onProgressChanged(WebView view, int progress) {
                // Activities and WebViews measure progress with different scales.
                // The progress meter will automatically disappear when we reach 100%
                setProgress(progress * 1000);
            }
        });
        url_stimulus_web_view.setWebViewClient(new WebViewClient());
        url_stimulus_web_view.getSettings().setJavaScriptEnabled(true);
        url_stimulus_web_view.loadUrl(intent.getStringExtra("url"));
    }
}
