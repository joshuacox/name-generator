// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v4.5.0/contracts/access/Ownable.sol";

contract NameGenerator is Ownable {
    // Mapping for storing available nouns and adjectives
    mapping(bytes32 => bool) public nounExists;
    mapping(bytes32 => bool) public adjectiveExists;

    bytes32[] public nounsList;
    bytes32[] public adjectivesList;

    string public SEPARATOR;
    uint16 public COUNT_O = 24; // Default number of lines

    address public immutable OWNER; // Contract owner for debug operations

    event NameGenerated(bytes32 adjective, bytes32 noun);
    event DebugInfo(bytes32 adjective, bytes32 noun);

    constructor() {
        OWNER = msg.sender;
        SEPARATOR = "-";
    }

    modifier onlyOwner() {
        require(msg.sender == OWNER, "Only the owner can call this function");
        _;
    }

    // Add a new noun to the list
    function addNoun(bytes32 noun) public onlyOwner {
        require(!nounExists[noun], "Noun already exists");
        nounsList.push(noun);
        nounExists[noun] = true;
    }

    // Add a new adjective to the list
    function addAdjective(bytes32 adjective) public onlyOwner {
        require(!adjectiveExists[adjective], "Adjective already exists");
        adjectivesList.push(adjective);
        adjectiveExists[adjective] = true;
    }

    // Generate a random name using stored nouns and adjectives
    function generateName() public view returns(string memory) {
        require(nounsList.length > 0 && adjectivesList.length > 0, 
            "No nouns or adjectives available");

        bytes32 noun = randomNoun();
        bytes32 adjective = randomAdjective();

        if (getDebug()) {
            emit DebugInfo(adjective, noun);
        }

        string memory combined = 
            string(abi.encodePacked(adjective)) 
          + SEPARATOR 
          + string(abi.encodePacked(noun));

        emit NameGenerated(adjective, noun);
        return combined;
    }

    // Simple RNG - NOT SECURE FOR PRODUCTION USE
    function randomNoun() private view returns(bytes32) {
        uint256 randomness = 
            block.timestamp + 
            block.number + 
            blockhash(block.number - 1) +
            ++counter;

        return nounsList[uint256(randomness) % nounsList.length];
    }

    // Simple RNG - NOT SECURE FOR PRODUCTION USE
    function randomAdjective() private view returns(bytes32) {
        uint256 randomness = 
            block.timestamp + 
            block.number + 
            blockhash(block.number - 1) +
            ++counter;

        return adjectivesList[uint256(randomness) % adjectivesList.length];
    }

    // Debug flag
    function getDebug() private view returns(bool) {
        // Solidity doesn't have direct env vars, so we could use a stored variable instead
        // For this example, let's assume debug is controlled via contract owner only
        return false; // Implement your own logic here
    }

    uint256 counter = 0;
}
