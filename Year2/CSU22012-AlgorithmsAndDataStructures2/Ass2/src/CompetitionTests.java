import static org.junit.Assert.*;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

@RunWith(JUnit4.class)
public class CompetitionTests {

  @Test
  public void testDijkstraConstructor() {

    assertNotNull("Dijkstra Constuctor - null filename",
                  new CompetitionDijkstra(null, 1, 2, 3));
    assertNotNull("Dijkstra Constuctor - empty filename",
                  new CompetitionDijkstra("", 1, 2, 3));
    assertNotNull("Dijkstra Constuctor - empty file",
                  new CompetitionDijkstra("input-J.txt", 1, 2, 3));
    assertNotNull("Dijkstra Constuctor - small file",
                  new CompetitionDijkstra("tinyEWD.txt", 1, 2, 3));
    assertNotNull("Dijkstra Constuctor - huge file",
                  new CompetitionDijkstra("input-H.txt", 1, 2, 3));
  }

  @Test
  public void testDijkstraContestantSpeedInput() {

    CompetitionDijkstra competition =
        new CompetitionDijkstra("tinyEWD.txt", 49, 50, 50);
    assertEquals("Dijkstra Contestant Speed Input - 49, 50, 50", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("tinyEWD.txt", 101, 50, 50);
    assertEquals("Dijkstra Contestant Speed Input - 101, 50, 50", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("tinyEWD.txt", 50, 49, 50);
    assertEquals("Dijkstra Contestant Speed Input - 50, 49, 50", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("tinyEWD.txt", 50, 101, 50);
    assertEquals("Dijkstra Contestant Speed Input - 50, 101, 50", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("tinyEWD.txt", 50, 50, 49);
    assertEquals("Dijkstra Contestant Speed Input - 50, 50, 49", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("tinyEWD.txt", 50, 50, 101);
    assertEquals("Dijkstra Contestant Speed Input - 50, 50, 101", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("tinyEWD.txt", 50, 50, 50);
    assertEquals("Dijkstra Contestant Speed Input - 50, 50, 50", 38,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("tinyEWD.txt", 100, 100, 100);
    assertEquals("Dijkstra Contestant Speed Input - 100, 100, 100", 19,
                 competition.timeRequiredforCompetition());
  }

  @Test
  public void testDijkstra() {

    CompetitionDijkstra competition =
        new CompetitionDijkstra("input-A.txt", 100, 100, 100);
    assertEquals("Dijksta - input-A.txt, 100, 100, 100", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("input-B.txt", 50, 60, 70);
    assertEquals("Dijksta - input-B.txt, 50, 60, 70", 10000,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("input-B.txt", 90, 75, 80);
    assertEquals("Dijksta - input-B.txt, 90, 75, 80", 6667,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("input-B.txt", 100, 100, 60);
    assertEquals("Dijksta - input-B.txt, 100, 100, 60", 8334,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("input-J.txt", 100, 100, 100);
    assertEquals("Dijksta - input-J.txt, 100, 100, 100", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionDijkstra("input-H.txt", 100, 100, 100);
    assertEquals("Dijksta - input-H.txt, 100, 100, 100", -1,
                 competition.timeRequiredforCompetition());
  }

  @Test
  public void testFWConstructor() {

    assertNotNull("FW Constuctor - null filename",
                  new CompetitionFloydWarshall(null, 1, 2, 3));
    assertNotNull("FW Constuctor - empty filename",
                  new CompetitionFloydWarshall("", 1, 2, 3));
    assertNotNull("FW Constuctor - empty file",
                  new CompetitionFloydWarshall("input-J.txt", 1, 2, 3));
    assertNotNull("FW Constuctor - small file",
                  new CompetitionFloydWarshall("tinyEWD.txt", 1, 2, 3));
    assertNotNull("FW Constuctor - huge file",
                  new CompetitionFloydWarshall("input-H.txt", 1, 2, 3));
  }

  @Test
  public void testFWContestantSpeedInput() {

    CompetitionFloydWarshall competition =
        new CompetitionFloydWarshall("tinyEWD.txt", 49, 50, 50);
    assertEquals("FW Contestant Speed Input - 49, 50, 50", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("tinyEWD.txt", 101, 50, 50);
    assertEquals("FW Contestant Speed Input - 101, 50, 50", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("tinyEWD.txt", 50, 49, 50);
    assertEquals("FW Contestant Speed Input - 50, 49, 50", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("tinyEWD.txt", 50, 101, 50);
    assertEquals("FW Contestant Speed Input - 50, 101, 50", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("tinyEWD.txt", 50, 50, 49);
    assertEquals("FW Contestant Speed Input - 50, 50, 49", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("tinyEWD.txt", 50, 50, 101);
    assertEquals("FW Contestant Speed Input - 50, 50, 101", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("tinyEWD.txt", 50, 50, 50);
    assertEquals("FW Contestant Speed Input - 50, 50, 50", 38,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("tinyEWD.txt", 100, 100, 100);
    assertEquals("FW Contestant Speed Input - 100, 100, 100", 19,
                 competition.timeRequiredforCompetition());
  }

  @Test
  public void testFW() {

    CompetitionFloydWarshall competition =
        new CompetitionFloydWarshall("input-A.txt", 100, 100, 100);
    assertEquals("FW - input-A.txt, 100, 100, 100", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("input-B.txt", 50, 60, 70);
    assertEquals("FW - input-B.txt, 50, 60, 70", 10000,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("input-B.txt", 90, 75, 80);
    assertEquals("FW - input-B.txt, 90, 75, 80", 6667,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("input-B.txt", 100, 100, 60);
    assertEquals("FW - input-B.txt, 100, 100, 60", 8334,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("input-J.txt", 100, 100, 100);
    assertEquals("FW - input-J.txt, 100, 100, 100", -1,
                 competition.timeRequiredforCompetition());

    competition = new CompetitionFloydWarshall("input-H.txt", 100, 100, 100);
    assertEquals("FW - input-H.txt, 100, 100, 100", -1,
                 competition.timeRequiredforCompetition());
  }
}
