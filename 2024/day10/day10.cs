
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

namespace Day10 {

    class Graph<T> {
        List<T> nodeData;
        List<HashSet<int>> outgoing;
        List<HashSet<int>> incoming;

        public Graph() {
            this.nodeData = new();
            this.outgoing = new();
            this.incoming = new();
        }
        
        public int AddNode(T data) {
            this.nodeData.Add(data);
            this.outgoing.Add(new());
            this.incoming.Add(new());
            return this.nodeData.Count - 1;
        }

        public T GetData(int nodeIndex) {
            return this.nodeData[nodeIndex];
        }
        
        public void AddEdge(int nodeIndex1, int nodeIndex2) {
            this.outgoing[nodeIndex1].Add(nodeIndex2);
            this.incoming[nodeIndex2].Add(nodeIndex1);
        }

        public bool hasPath(int nodeStart, int nodeEnd) {
            List<bool> visited = new(Enumerable.Repeat(false, this.nodeData.Count));
            Stack<int> nodes = new();
            nodes.Push(nodeStart);
            
            while (nodes.Count > 0) {
                int nodeId = nodes.Pop();
                if (nodeId == nodeEnd) { return true; }
                visited[nodeId] = true;
                
                foreach (int nodeId2 in this.outgoing[nodeId]) {
                    if (!visited[nodeId2]) { nodes.Push(nodeId2); }
                }
            }
            return false;
        }

        public int countPaths(int nodeStart, int nodeEnd) {
            List<int> pathCounts = new(Enumerable.Repeat(0, this.nodeData.Count));
            List<bool> visited = new(Enumerable.Repeat(false, this.nodeData.Count));
            Queue<int> nodes = new();
            nodes.Enqueue(nodeEnd);
            pathCounts[nodeEnd] = 1;
            visited[nodeEnd] = true;

            while (nodes.Count > 0) {
                int nodeId = nodes.Dequeue();
                foreach (int nodeId2 in this.incoming[nodeId]) {
                    pathCounts[nodeId2] += pathCounts[nodeId];
                    if (!visited[nodeId2]) { nodes.Enqueue(nodeId2); }
                    visited[nodeId2] = true;
                }
            }
            return pathCounts[nodeStart];
        }
    }
    
    class Day10Solution {

        Graph<int> trailGraph;
        List<int> trailHeadNodes;
        List<int> trailTailNodes;
        List<List<int>> nodeIds;
        
        Day10Solution(string filename) {
            this.trailGraph = new();
            this.nodeIds = new();
            this.trailHeadNodes = new();
            this.trailTailNodes = new();
            this.parseInput(filename);
        }

        private void parseInput(string filename) {
            StreamReader reader = new StreamReader(filename);
            
            string line = reader.ReadLine();
            while (line != null) {

                List<int> row = new();
                foreach (char c in line.ToCharArray()) {
                    int elevation = (int)(c - '0');
                    int nodeId = this.trailGraph.AddNode(elevation);

                    if (elevation == 0) { this.trailHeadNodes.Add(nodeId); }
                    if (elevation == 9) { this.trailTailNodes.Add(nodeId); }
                    
                    row.Add(nodeId);
                }
                this.nodeIds.Add(row);
                
                line = reader.ReadLine();
            }
            reader.Close();
            
            // create edges
            for (int r = 0; r < this.nodeIds.Count; r++) {
                for (int c = 0; c < this.nodeIds.Count; c++) {
                    int nodeId = this.nodeIds[r][c];
                    int elevation = this.trailGraph.GetData(nodeId);

                    if (r - 1 >= 0) { // north neighbor exists
                        int nodeIdN = this.nodeIds[r - 1][c];
                        int elevationN = this.trailGraph.GetData(nodeIdN);
                        if (elevationN - elevation == 1) { this.trailGraph.AddEdge(nodeId, nodeIdN); }
                    }
                    if (r + 1 < this.nodeIds.Count) { // south neighbor exists
                        int nodeIdS = this.nodeIds[r + 1][c];
                        int elevationS = this.trailGraph.GetData(nodeIdS);
                        if (elevationS - elevation == 1) { this.trailGraph.AddEdge(nodeId, nodeIdS); }
                    }
                    if (c - 1 >= 0) { // west neighbor exists
                        int nodeIdW = this.nodeIds[r][c - 1];
                        int elevationW = this.trailGraph.GetData(nodeIdW);
                        if (elevationW - elevation == 1) { this.trailGraph.AddEdge(nodeId, nodeIdW); }
                    }
                    if (c + 1 < this.nodeIds[r].Count) { // east neighbor exists
                        int nodeIdE = this.nodeIds[r][c + 1];
                        int elevationE = this.trailGraph.GetData(nodeIdE);
                        if (elevationE - elevation == 1) { this.trailGraph.AddEdge(nodeId, nodeIdE); }
                    }
                }
            }
        }

        public int solvePart1() {
            int scoreSum = 0;
            foreach (int trailHeadNode in this.trailHeadNodes) {
                int score = 0;
                foreach (int trailTailNode in this.trailTailNodes) {
                    if (this.trailGraph.hasPath(trailHeadNode, trailTailNode)) {
                        score++;
                    }
                }
                scoreSum += score;
            }

            return scoreSum;
        }

        public int solvePart2() {
            int ratingSum = 0;
            foreach (int trailHeadNode in this.trailHeadNodes) {
                int rating = 0;
                foreach (int trailTailNode in this.trailTailNodes) {
                    rating += this.trailGraph.countPaths(trailHeadNode, trailTailNode);
                }
                ratingSum += rating;
            }
            return ratingSum;
        }
        
        static void Main(string[] args) {

            Day10Solution day10 = new Day10Solution("input.txt");

            int scoreSum = day10.solvePart1();
            Console.WriteLine(scoreSum);
            
            int ratingSum = day10.solvePart2();
            Console.WriteLine(ratingSum);

            Console.ReadKey();
        }
    }
}
