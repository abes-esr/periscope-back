package fr.abes.periscope.core.entity;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public interface NoticeSolr {

    String ppn = "";
    String issn = "";
    Set<String> pcpList = new HashSet<>();
    Set<String> rcrList = new HashSet<>();
    String editor = "";
    String processingGlobalData = "";
    String keyTitle = "";
    String keyShortedTitle = "";
    String properTitle = "";
    String titleFromDifferentAuthor = "";
    String parallelTitle = "";
    String titleComplement = "";
    String sectionTitle = "";
    String keyTitleQualifer = "";
    String continiousType = "";
    List<String> externalURLs = new ArrayList<>();
    Integer nbLocation = 0;

    public String getPpn();
    public String getIssn();
    public Set<String> getPcpList();
    public Set<String> getRcrList();
    public String getEditor();
    public String getProcessingGlobalData();
    public String getKeyTitle();
    public String getKeyShortedTitle();
    public String getProperTitle();
    public String getTitleFromDifferentAuthor();
    public String getParallelTitle();
    public String getTitleComplement();
    public String getSectionTitle();
    public String getKeyTitleQualifer();
    public String getContiniousType();
    public List<String> getExternalURLs();
    public Integer getNbLocation();
}
